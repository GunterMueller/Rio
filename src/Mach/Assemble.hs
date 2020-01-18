{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE DataKinds, RecursiveDo #-}
module Mach.Assemble (assembleProg) where

import Data.Foldable

import Syntax (Var(..))

import Gm.Machine
import Mach.Asm

compileGInst :: GmCode -> BlockBuilder ()
compileGInst (Push (Global (Var v))) = push (label v)

compileGInst (Push (Arg i)) = do
  rax =<- (i + 1) * 8 `quadOff` rsp
  push (arg_off `quadOff` rax)

compileGInst (Push (Local i)) = do
  push $ i * 8 `quadOff` rsp

compileGInst (Push (Value i)) = pushInt (int64 i)

compileGInst MkAp = do
  mov (int8 tag_AP) (tag_off `byteOff` hp)
  pop (arg_off `quadOff` hp)
  pop (fun_off `quadOff` hp)
  push hp
  hp += int64 valueSize

compileGInst (Update n) = do
  pop rax
    -- Store new root of graph in a temporary register
  rbx =<- n * 8 `quadOff` rsp
    -- Load the address of the n'th value on the stack into r13
  0 `byteOff` rbx =<- int8 tag_LINK
    -- Replace the tag of the n'th value on the stack with the
    -- indirection tag
  link_off `quadOff` rbx =<- rax
    -- do the link!

compileGInst (Pop n) = add (int64 (n * 8)) rsp

compileGInst (Slide n) = do
  pop rax
  rsp += int64 ((n - 1) * 8)
  push rax

compileGInst (Alloc n) =
  for_ [1..n] $ \_ -> do
    push hp
    hp += int64 valueSize

compileGInst Unwind = jmp (Label "unwind")
compileGInst Eval = call "eval"

compileGInst Add = compileBinaryInst add
compileGInst Sub = compileBinaryInst sub

compileGInst Mul = do
  pop rbx
  rax =<- intv_off `quadOff` rbx
  pop rbx
  mul (intv_off `quadOff` rbx)
  pushInt rax

compileGInst Div = do
  rdx ^= rdx
  pop rbx
  rax =<- intv_off `quadOff` rbx -- rax=denominator
  pop rbx
  idiv (intv_off `quadOff` rbx)  -- intv_off[rbx]=numerator
  pushInt rax

compileGInst Equ = do
  rdx ^= rdx
  pop rbx
  rax =<- intv_off `quadOff` rbx
  pop rbx
  cmp (intv_off `quadOff` rbx) rax
  mov (int64 1) rax
  cmovne rax rdx
  pushInt rdx

compileGInst (Cond c_then c_else) = do
  pop rbx
  cmp (int64 0) (intv_off `quadOff` rbx)
  rec
    jne else_label
    traverse_ compileGInst c_then
    jmp exit_label
    else_label <- genLabel
    traverse_ compileGInst c_else
    exit_label <- genLabel
  pure ()

compileBinaryInst :: (Operand 'S64 -> Operand 'S64 -> BlockBuilder ()) -> BlockBuilder ()
compileBinaryInst k = do
  pop rbx -- top number
  mov (intv_off `quadOff` rbx) rsi
  pop rbx -- snd number
  -- behavior: push (snd `op` top)
  --                 ^ src    ^ dest
  mov (intv_off `quadOff` rbx) rdi
  k rsi rdi
  pushInt rdi

pushInt :: Operand 'S64 -> BlockBuilder ()
pushInt val = do
  tag_off  `byteOff` hp =<- int8 tag_INT
  intv_off `quadOff` hp =<- val
  push hp
  hp += int64 valueSize

entry :: Foldable f => f GmCode -> BlockBuilder ()
entry code
  | bytes_alloced > 0
  = do lea (bytes_alloced `quadOff` hp) r10
       cmp hpLim r10
       ja (Label "collect_garbage")
  | otherwise = pure ()
  where
    bytes_alloced = foldl' cntBytes 0 code
    cntBytes x MkAp = valueSize + x
    cntBytes x (Push (Value _)) = valueSize + x
    cntBytes x (Alloc n) = n * valueSize + x
    cntBytes x (Cond xs ys) = foldl' cntBytes 0 xs + foldl' cntBytes 0 ys + x
    cntBytes x _ = x

assembleProg :: [GmSc] -> [Section]
assembleProg scs =
  let (blocks, records) = unzip (map assembleSc scs)
   in [Text blocks, Data records]

valueSize :: Num a => a
valueSize = 24

tag_SC, tag_AP, tag_LINK, tag_INT :: Num a => a
tag_LINK = 1
tag_SC   = 2
tag_AP   = 3
tag_INT  = 4

tag_off, link_off, intv_off, fun_off, arg_off, code_off, arity_off :: Num a => a
tag_off    = 0
link_off   = 8
intv_off   = 8
fun_off    = 8
arg_off    = 16
code_off   = 8
arity_off  = 16

hp, hpLim :: Operand 'S64
hp    = r14
hpLim = r15

assembleSc :: GmSc -> (Block, Record)
assembleSc (Supercomb (Var name) arity code) =
  let instr x = comment (show x) *> compileGInst x
      block = buildBlock (name <> "_entry") (entry code *> traverse instr code)
   in ( block
      , Record name
          [ Byte   (tag_SC:replicate 7 0)
          , Bytes8 [ Lbl (name <> "_entry") ]
          , Bytes4 [ Int (negate ((arity + 1) * 8)) ]
          ]
      )

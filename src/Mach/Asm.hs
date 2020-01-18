{-# LANGUAGE PolyKinds, DataKinds, GADTs, ExistentialQuantification,
   ScopedTypeVariables, GeneralisedNewtypeDeriving, DerivingStrategies, FlexibleInstances, MultiParamTypeClasses #-}
module Mach.Asm
  ( Block(..), Record(..), Section(..), Directive(..), ConstOp(..), Operand
  , Size(..), Target(..), Register(..)
  , comment
  , push, pop, idiv, mul
  , mov, lea
  , add, sub, xor, cmp
  , (=<-), (+=), (-=), (^=)
  , jmp, jne, je, jna, ja, jz, jnz, call
  , cmove, cmovne, cmova, cmovna, cmovz, cmovnz

  , rax, rbx, rcx, rdx, rdi, rsi, rsp, rbp, r10, r11, r12, r13, r14, r15
  , eax, ebx, ecx, edx, edi, esi, esp, ebp, r10d, r11d, r12d, r13d, r14d, r15d

  , int8, int16, int32, int64, label
  , byteOff, wordOff, longOff, quadOff
  , r, genLabel
  , BlockBuilder, buildBlock
  )
  where

import Language.Haskell.TH.Quote
import Language.Haskell.TH

import Control.Monad.Writer.Strict
import Control.Monad.State

import Data.Proxy
import Data.List

data Size = S8 | S16 | S32 | S64
  deriving (Eq, Show, Ord)

class HasSize s where
  reflect :: proxy s -> Size
  suffix  :: proxy s -> String

instance HasSize 'S8 where
  reflect _ = S8
  suffix _ = "b"

instance HasSize 'S16 where
  reflect _ = S16
  suffix _ = "w"

instance HasSize 'S32 where
  reflect _ = S32
  suffix _ = "l"

instance HasSize 'S64 where
  reflect _ = S64
  suffix _ = "q"

instance HasSize s => HasSize (Proxy s) where
  reflect (_ :: proxy (Proxy s)) = reflect (Proxy :: Proxy s)
  suffix (_ :: proxy (Proxy s)) = suffix (Proxy :: Proxy s)

data Insn
  = forall s. HasSize s => Mov  (Operand s) (Operand s)
  | forall s. HasSize s => Xor  (Operand s) (Operand s)
  | forall s. HasSize s => Cmp  (Operand s) (Operand s)
  | forall s. HasSize s => Lea  (Operand s) (Operand s)
  | forall s. HasSize s => Add  (Operand s) (Operand s)
  | forall s. HasSize s => Sub  (Operand s) (Operand s)
  | forall s. HasSize s => Idiv (Operand s)
  | forall s. HasSize s => Push (Operand s)
  | forall s. HasSize s => Mul  (Operand s)
  | forall s. HasSize s => Pop  (Operand s)
  | forall s. HasSize s => Cmov Cond (Operand s) (Operand s)
  | Jmp Target | J Cond Target
  | Call String
  | Comment String
  | LocalLabel String

data Cond
  = E | Ne | Z | Nz | A | Na
  deriving (Eq, Ord)

instance Show Cond where
  show E  = "e"
  show Ne = "ne"
  show Z  = "z"
  show Nz = "nz"
  show A  = "a"
  show Na = "na"

instance Show Insn where
  show (Call x)       = "call " ++ x
  show (Jmp t)        = "jmp"   ++ suf t  ++ show t
  show (J c t)        = 'j':show c ++ suf t ++ show t
  show (Pop op)       = "pop"   ++ suffix op ++ " " ++ show op
  show (Push op)      = "push"  ++ suffix op ++ " " ++ show op
  show (Idiv op)      = "push"  ++ suffix op ++ " " ++ show op
  show (Mul op)       = "mul"   ++ suffix op ++ " " ++ show op
  show (Mov op op2)   = "mov"   ++ suffix op ++ " " ++ show op ++ ',':' ':show op2
  show (Lea op op2)   = "lea"   ++ suffix op ++ " " ++ show op ++ ',':' ':show op2
  show (Cmp op op2)   = "cmp"   ++ suffix op ++ " " ++ show op ++ ',':' ':show op2
  show (Add op op2)   = "add"   ++ suffix op ++ " " ++ show op ++ ',':' ':show op2
  show (Sub op op2)   = "sub"   ++ suffix op ++ " " ++ show op ++ ',':' ':show op2
  show (Xor op op2)   = "xor"   ++ suffix op ++ " " ++ show op ++ ',':' ':show op2
  show (Cmov c op op2)
    = "cmov" ++ show c ++ suffix op ++ " " ++ show op ++ ',':' ':show op2
  show (Comment s)    = '#':' ':s
  show (LocalLabel s) = s ++ ":"

suf :: Target -> String
suf (Indirect _) = "q "
suf _ = " "

data Operand s
  = R (Register s)
  | forall r. HasSize r => RD Int (Register r)
  | Immediate ConstOp

instance HasSize s => HasSize (Operand s) where
  reflect (_ :: proxy (Operand s)) = reflect (Proxy :: Proxy s)
  suffix (_ :: proxy (Operand s)) = suffix (Proxy :: Proxy s)

instance HasSize s => Show (Operand s) where
  show (R x) = show x
  show (RD dis x) = show dis ++ "(" ++ show x ++ ")"
  show (Immediate i) = '$':show i

data Register s = Rax | Rbx | Rcx | Rdx | Rdi | Rsi | Rsp | Rbp | R10 | R11 | R12 | R13 | R14 | R15
  deriving (Eq)

instance HasSize s => Show (Register s) where
  show reg = '%':go (id reg) (reflect (Proxy :: Proxy s)) where
    go r s =
      case s of
        S8 -> r ++ "l"
        S16 -> if length r == 1 then r ++ "x" else r
        S32 -> "e" ++ go r S16
        S64 -> "r" ++ go r S16
    id Rax = "a"
    id Rbx = "b"
    id Rcx = "c"
    id Rdx = "d"
    id Rdi = "di"
    id Rsi = "si"
    id Rsp = "sp"
    id Rbp = "bp"
    id R10 = "10"
    id R11 = "11"
    id R12 = "12"
    id R13 = "13"
    id R14 = "14"
    id R15 = "15"

rax, rbx, rcx, rdx, rdi, rsi, rsp, rbp, r10, r11, r12, r13, r14, r15 :: Operand 'S64
rax = R Rax
rbx = R Rbx
rcx = R Rcx
rdx = R Rdx
rdi = R Rdi
rsi = R Rsi
rsp = R Rsp
rbp = R Rbp
r10 = R R10
r11 = R R11
r12 = R R12
r13 = R R13
r14 = R R14
r15 = R R15

eax, ebx, ecx, edx, edi, esi, esp, ebp, r10d, r11d, r12d, r13d, r14d, r15d :: Operand 'S32
eax = R Rax
ebx = R Rbx
ecx = R Rcx
edx = R Rdx
edi = R Rdi
esi = R Rsi
esp = R Rsp
ebp = R Rbp
r10d = R R10
r11d = R R11
r12d = R R12
r13d = R R13
r14d = R R14
r15d = R R15

int8 :: Int -> Operand 'S8
int8 = Immediate . Int

int16 :: Int -> Operand 'S16
int16 = Immediate . Int

int32 :: Int -> Operand 'S32
int32 = Immediate . Int

int64 :: Int -> Operand 'S64
int64 = Immediate . Int

label :: String -> Operand 'S64
label = Immediate . Lbl

byteOff :: HasSize r => Int -> Operand r -> Operand 'S8
byteOff i (RD i' r') = RD (i + i') r'
byteOff i (R reg)    = RD i reg
byteOff _ _ = error "byteOff: not a register"

wordOff :: HasSize r => Int -> Operand r -> Operand 'S16
wordOff i (RD i' r') = RD (i + i') r'
wordOff i (R reg)    = RD i reg
wordOff _ _ = error "wordOff: not a register"

longOff :: HasSize r => Int -> Operand r -> Operand 'S32
longOff i (RD i' r') = RD (i + i') r'
longOff i (R reg)    = RD i reg
longOff _ _ = error "longOff: not a register"

quadOff :: HasSize r => Int -> Operand r -> Operand 'S64
quadOff i (RD i' r') = RD (i + i') r'
quadOff i (R reg)    = RD i reg
quadOff _ _ = error "quadOff: not a register"

infixl 6 `byteOff`
infixl 6 `wordOff`
infixl 6 `longOff`
infixl 6 `quadOff`

data Target
  = Label String | Indirect (Operand 'S64)

instance Show Target where
  show (Label x) = x
  show (Indirect op) = '*':show op

data Directive = Bytes8 [ConstOp] | Bytes4 [ConstOp] | Byte [ConstOp]
  deriving (Eq)

data ConstOp = Int Int | Lbl String
  deriving (Eq)

instance Num ConstOp where
  fromInteger = Int . fromInteger
  (+) = undefined
  (-) = undefined
  (*) = undefined
  abs = undefined
  signum = undefined

instance Show ConstOp where
  show (Int i) = show i
  show (Lbl x) = x

instance Show Directive where
  show (Bytes8 x) = ".8byte " ++ concat (intersperse ", " (map show x))
  show (Bytes4 x) = ".4byte " ++ concat (intersperse ", " (map show x))
  show (Byte x)   = ".byte "  ++ concat (intersperse ", " (map show x))

data Block =
  Block { blockLabel  :: String
        , blockIsns   :: [Insn]
        }

instance Show Block where
  show (Block nm insns) = unlines ((nm ++ ":") : map (indent . show) insns) where
    indent [] = []
    indent xs
      | last xs == ':' = xs
      | otherwise = ' ': ' ':xs

data Record =
  Record { recordLabel :: String
         , recordIsns  :: [Directive]
         }
  deriving (Eq)

instance Show Record where
  show (Record l insns) = unlines ((l ++ ":") : map (("  " ++) . show) insns)

data Section
  = Data   [Record]
  | Text   [Block]
  | Rodata [Record]

instance Show Section where
  show (Data rs) = unlines (".section .data":map show rs)
  show (Text rs) = unlines (".section .text":map show rs)
  show (Rodata rs) = unlines (".section .rodata":map show rs)

r :: QuasiQuoter
r = QuasiQuoter {
    quoteExp  = return . LitE . StringL,
    quotePat  = \_ -> fail "illegal raw string QuasiQuote (allowed as expression only, used as a pattern)",
    quoteType = \_ -> fail "illegal raw string QuasiQuote (allowed as expression only, used as a type)",
    quoteDec  = \_ -> fail "illegal raw string QuasiQuote (allowed as expression only, used as a declaration)"
}

data BbState = BbState { off :: !Int, nextLabel :: !Int, blockName :: String }
  deriving (Eq, Show, Ord)

newtype BlockBuilder a = Bb { runBlockBuilder :: StateT BbState (Writer (Endo [Insn])) a }
  deriving newtype ( MonadState BbState, MonadWriter (Endo [Insn])
                   , Functor, Applicative, Monad, MonadFix
                   )

buildBlock :: String -> BlockBuilder a -> Block
buildBlock name comp =
  let ((_, _), writer) = runWriter (runStateT (runBlockBuilder comp) empty)
      empty = BbState 1 0 name
   in Block name (appEndo writer [])

comment :: String -> BlockBuilder ()
comment x = tell (Endo (Comment x:))

push, pop, idiv, mul :: HasSize s => Operand s -> BlockBuilder ()
push x = insn tell (Endo (Push x:))
idiv x = insn tell (Endo (Idiv x:))
mul b  = insn tell (Endo (Mul b:))
pop x  = insn tell (Endo (Pop x:))

jmp, je, jne, ja, jna, jz, jnz :: Target -> BlockBuilder ()
jmp x = insn tell (Endo (Jmp x:))
je x  = insn tell (Endo (J E x:))
jne x = insn tell (Endo (J Ne x:))
ja x  = insn tell (Endo (J A  x:))
jna x = insn tell (Endo (J Na x:))
jz x  = insn tell (Endo (J Z  x:))
jnz x = insn tell (Endo (J Nz x:))

call :: String -> BlockBuilder ()
call x = tell (Endo (Call x:))

mov, lea, cmp, add, sub, xor :: HasSize s => Operand s -> Operand s -> BlockBuilder ()
mov a b = insn tell (Endo (Mov a b:))
lea a b = insn tell (Endo (Lea a b:))
cmp a b = insn tell (Endo (Cmp a b:))
add a b = insn tell (Endo (Add a b:))
sub a b = insn tell (Endo (Sub a b:))
xor a b = insn tell (Endo (Xor a b:))

cmove, cmovne, cmova, cmovna, cmovz, cmovnz :: HasSize s => Operand s -> Operand s -> BlockBuilder ()
cmove x y  = insn tell (Endo (Cmov E  x y:))
cmovne x y = insn tell (Endo (Cmov Ne x y:))
cmova x y  = insn tell (Endo (Cmov A  x y:))
cmovna x y = insn tell (Endo (Cmov Na x y:))
cmovz x y  = insn tell (Endo (Cmov Z  x y:))
cmovnz x y = insn tell (Endo (Cmov Nz x y:))

(+=), (-=), (=<-), (^=) :: HasSize s => Operand s -> Operand s -> BlockBuilder ()
dest += source  = insn tell (Endo (Add source dest:))
dest -= source  = insn tell (Endo (Sub source dest:))
dest ^= source  = insn tell (Endo (Xor source dest:))
dest =<- source = insn tell (Endo (Mov source dest:))

insn :: (a -> BlockBuilder b) -> a -> BlockBuilder ()
insn k x = do
  _ <- k x
  BbState off l m <- get
  put $! BbState (off + 1) l m

genLabel :: BlockBuilder Target
genLabel = do
  BbState off label nm <- get
  let lname = ".L" ++ nm ++ "." ++ show label
  put $! BbState (off + 1) (label + 1) nm
  insn tell (Endo (LocalLabel lname:))
  pure (Label lname)

infix 5 =<-
infix 5 +=

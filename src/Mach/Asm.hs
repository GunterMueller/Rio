{-# LANGUAGE PolyKinds, DataKinds, GADTs, ExistentialQuantification,
   ScopedTypeVariables #-}
module Mach.Asm
  ( Block(..), Record(..), Section(..), Directive(..), ConstOp(..), Operand
  , Size(..), Target(..), Register(..)
  , comment
  , push, pop
  , mov, lea
  , add, cmp
  , jmp, jne, je, jna, ja, call
  , rax, rbx, rcx, rdx, rdi, rsi, rsp, rbp, r10, r11, r12, r13, r14, r15
  , eax, ebx, ecx, edx, edi, esi, esp, ebp, r10d, r11d, r12d, r13d, r14d, r15d
  , int8, int16, int32, int64, label
  , byteOff, wordOff, longOff, quadOff
  , r
  , BlockBuilder, buildBlock, (=<-), (+=)
  )
  where

import Language.Haskell.TH.Quote
import Language.Haskell.TH

import Control.Monad.Writer

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
  | forall s. HasSize s => Cmp  (Operand s) (Operand s)
  | forall s. HasSize s => Lea  (Operand s) (Operand s)
  | forall s. HasSize s => Add  (Operand s) (Operand s)
  | forall s. HasSize s => Push (Operand s)
  | forall s. HasSize s => Pop  (Operand s)
  | Jmp  Target | Ja Target | Jna Target | Je Target | Jne Target
  | Call String
  | Comment String

instance Show Insn where
  show (Call x)     = "call " ++ x
  show (Jmp t)      = "jmp"   ++ suf t  ++ show t
  show (Ja t)       = "ja"    ++ suf t  ++ show t
  show (Jna t)      = "jna"   ++ suf t  ++ show t
  show (Je t)       = "jz"    ++ suf t  ++ show t
  show (Jne t)      = "jnz"   ++ suf t  ++ show t
  show (Pop op)     = "pop"   ++ suffix op ++ " " ++ show op
  show (Push op)    = "push"  ++ suffix op ++ " " ++ show op
  show (Mov op op2) = "mov"   ++ suffix op ++ " " ++ show op ++ ',':' ':show op2
  show (Lea op op2) = "lea"   ++ suffix op ++ " " ++ show op ++ ',':' ':show op2
  show (Cmp op op2) = "cmp"   ++ suffix op ++ " " ++ show op ++ ',':' ':show op2
  show (Add op op2) = "add"   ++ suffix op ++ " " ++ show op ++ ',':' ':show op2
  show (Comment s)  = '#':' ':s

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
  Block { blockLabel :: String
        , blockIsns  :: [Insn]
        }

instance Show Block where
  show (Block l insns) = unlines ((l ++ ":") : map (("  " ++) . show) insns)

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

type BlockBuilder = Writer (Endo [Insn])

buildBlock :: String -> Writer (Endo [Insn]) a -> Block
buildBlock name comp =
  let insns = appEndo (execWriter comp) []
   in Block name insns

comment :: String -> Writer (Endo [Insn]) ()
comment x = tell (Endo (Comment x:))

push, pop :: HasSize s => Operand s -> Writer (Endo [Insn]) ()
push x = tell (Endo (Push x:))
pop x = tell (Endo (Pop x:))

jmp, je, jne, ja, jna :: Target -> Writer (Endo [Insn]) ()
jmp x = tell (Endo (Jmp x:))
je x  = tell (Endo (Je  x:))
jne x = tell (Endo (Jne x:))
ja x  = tell (Endo (Ja  x:))
jna x = tell (Endo (Jna x:))

call :: String -> Writer (Endo [Insn]) ()
call x = tell (Endo (Call x:))

mov, lea, cmp, add, (=<-), (+=) :: HasSize s => Operand s -> Operand s -> Writer (Endo [Insn]) ()
mov a b = tell (Endo (Mov a b:))
lea a b = tell (Endo (Lea a b:))
cmp a b = tell (Endo (Cmp a b:))
add a b = tell (Endo (Add a b:))

dest += source = tell (Endo (Add source dest:))
dest =<- source = tell (Endo (Mov source dest:))

infix 5 =<-
infix 5 +=

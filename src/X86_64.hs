module X86_64 where
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import Data.Word

import Data.Int
import  Control.Monad.Trans.Class

-- Target machine
type Addr = Word32

data Prim = PInt Int64 | PReg Register
                        deriving Show
type Assemble = WriterT [Word8] (State Word8)

emit :: [Word8] -> Assemble ()
emit x = tell x >> lift (modify (+ 1))

bytes :: Integral n => n -> [Word8]
bytes 0 = []
bytes i =  fromIntegral lastByte : bytes rest
    where (rest, lastByte) = quotRem i 256

imm :: Integral a => a -> Assemble ()
imm = emit . bytes

ret :: Assemble ()
ret = emit [0xc3]

push :: Register -> Assemble ()
push r = emit [0x50 + reg r]

pop :: Register -> Assemble ()
pop r = emit [0x58 + reg r]

prologue :: Assemble ()
prologue = undefined


epilogue :: Assemble ()
epilogue = undefined

data Bits = B64 | B32 | B16 | BH8 | BL8 deriving (Show)

data Register =   RAX | EAX  | AX | AH |  AL -- Accumulator
                | RBX | EBX  | BX | BH |  BL -- Base index (for use with arrays)
                | RCX | ECX  | CX | CH |  CL -- Counter (for use with loops and strings)
                | RDX | EDX  | DX | DH |  DL -- Extend the precision of the accumulator (e.g. combine 32-bit EAX and EDX for 64-bit integer operations in 32-bit code)


                | RSI | ESI  | SI | SIL -- Source index for string operations.
                | RDI | EDI  | DI | DIL -- Destination index for string operations.
                | RSP | ESP  | SP | SPL -- Stack pointer for top address of the stack.
                | RBP | EBP  | BP | BPL -- Stack base pointer for holding the address of the current stack frame.


                | R8  | R8D  | R8W  | R8B
                | R9  | R9D  | R9W  | R9B
                | R10 | R10D | R10W | R10B
                | R11 | R11D | R11W | R11B
                | R12 | R12D | R12W | R12B
                | R13 | R13D | R13W | R13B
                | R14 | R14D | R14W | R14B
                | R15 | R15D | R15W | R15B
                deriving (Show, Eq)

rax :: Word8
rax = reg RAX

rbx :: Word8
rbx = reg RBX

rcx :: Word8
rcx = reg RCX

rdx :: Word8
rdx = reg RDX

rsi :: Word8
rsi = reg RSI

rdi :: Word8
rdi = reg RDI

rsp :: Word8
rsp = reg RSP

rbp :: Word8
rbp = reg RBP

reg :: Register -> Word8
reg x = case x of
        RAX -> 0
        RCX -> 1
        RDX -> 2
        RBX -> 3
        RSP -> 4
        RBP -> 5
        RSI -> 6
        RDI -> 7

type Program = [Instr]
data Instr = Push Register
        | Pop Register
        | Set Register Prim
        | Add Register Prim
        | Mul Register Prim
        | Xor Register Prim
        | Or Register Prim
        | Call Addr
        | Ret
        deriving Show


volatile :: Register -> Bool
volatile x = x `notElem`   [R12, R12D, R12W, R12B,
                            R13, R13D, R13W, R13B,
                            R14, R14D, R14W, R14B,
                            R15, R15D, R15W, R15B,
                            RDI, EDI , DI, DIL,
                            RSI, ESI , SI, SIL,
                            RBX, EBX , BX, BH, BL,
                            RBP, EBP , BP, BPL,
                            RSP, ESP , SP, SPL]
matchBits :: Int -> Bits
matchBits v | v < 255  = BL8
matchBits v | v < 65535  = B16
matchBits v | v < 2147483647 = B32
matchBits v | v < 9223372036854775807 = B64

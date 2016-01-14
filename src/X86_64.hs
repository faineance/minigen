module X86_64 where
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import Data.Word
import  Control.Monad.Trans.Class

-- Target machine

type Assemble = WriterT [Word8] (State Word8)

emit :: Word8 -> Assemble ()
emit x = tell [x] >> lift (modify (+ 1))

-- bytes :: Integral n => n -> [n]
-- bytes 0 = []
-- bytes i = reverse . lastDigit : bytes rest
--     where (rest, lastDigit) = quotRem i 10

-- imm :: Integral a => a -> Assemble ()
-- imm x =  emit bytes x


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


type Program = [Instr]
data Instr = Push Register
        | Pop Register
        | Set Register Int
        | Add Register Register
        | Mul Register Register
        | Xor Register (Either Register Int)
        | Or Register (Either Register Int)
        | Label
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

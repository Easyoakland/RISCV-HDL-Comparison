module AluDecode where

import Clash.Prelude (Bool, Natural, Vec, bitCoerce, error, (!!))
import Types.AluCtrl (AluCtrl (..))
import Types.Newtype (Funct3 (..), Funct7 (..), Opcode (..))

aluDecodeT :: Opcode -> Funct7 -> Funct3 -> AluCtrl
aluDecodeT (Opcode op) (Funct7 fn7) (Funct3 fn3) = case op of
  -- https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#lui
  0b0110111 -> LUI
  -- Loads and stores perform adds due to offset calculation.
  -- https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#sw
  0b0100011 -> ADD
  -- https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#lw
  0b0000011 -> ADD
  -- i-type
  0b0010011 ->
    ( case fn3 of
        -- https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#andi
        0b111 -> AND
        -- https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#ori
        0b110 -> OR
        -- https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#addi
        0b000 -> ADD
        -- https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#xori
        0b100 -> XOR
        -- https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#slli
        0b001 -> SLL
        0b101 ->
          -- https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#srai
          if (bitCoerce fn7 :: Vec 7 Bool) !! (5 :: Natural)
            then SRA
            else -- https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#srli
              SRL
        -- https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#slti
        0b010 -> SLT
        -- https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#sltiu
        0b011 -> SLTU
        _ -> error "invalid fn3"
    )
  -- r-type
  0b0110011 ->
    ( case (fn7, fn3) of
        -- https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#and
        (0b0000000, 0b111) -> AND
        -- https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#or
        (0b0000000, 0b110) -> OR
        -- https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#add
        (0b0000000, 0b000) -> ADD
        -- https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#sll
        (0b0000000, 0b001) -> SLL
        -- https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#srl
        (0b0000000, 0b101) -> SRL
        -- https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#sub
        (0b0100000, 0b000) -> SUB
        -- https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#slt
        (0b0000000, 0b010) -> SLT
        -- https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#xor
        (0b0000000, 0b100) -> XOR
        -- https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#sltu
        (0b0000000, 0b011) -> SLTU
        -- https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#sra
        (0b0100000, 0b101) -> SRA
        _ -> error "invalid fn7, fn3"
    )
  _ -> error "invalid instr"

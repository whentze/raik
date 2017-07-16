use std::{fmt, ops};
use byteorder::{LittleEndian, ByteOrder};
use super::ProgramState;

#[derive(Debug, Clone, Copy)]
pub enum OpCode {
    ADDI,
    ANDI,
    ORI,
    XORI,
    SLTI,
    SLTIU,
    SRLI,
    SLLI,
    SRAI,
    ADD,
    SUB,
    SLL,
    SRL,
    SRA,
    OR,
    AND,
    LB,
    LH,
    LW,
    SB,
    SH,
    SW,
    JALR,
    JAL,
    BEQ,
    BNE,
    BLT,
    BLTU,
    BGE,
    BGEU,

    //M extension
    MUL,
}

#[derive(Debug, Clone, Copy)]
enum EncodingType {
    R,
    I,
    S,
    B,
    U,
    J,
    R4,
    None,
}

pub struct Instruction {
    inner: i32,
    opcode: OpCode,
}

impl Instruction {
    pub fn rd(&self) -> u8 {
        ((self.inner >> 7) & 0b11111) as u8
    }
    pub fn funct3(&self) -> u8 {
        ((self.inner >> 12) & 0b11111) as u8
    }
    pub fn funct2(&self) -> u8 {
        ((self.inner >> 25) & 0b11) as u8
    }
    pub fn rs1(&self) -> u8 {
        ((self.inner >> 15) & 0b11111) as u8
    }
    pub fn rs2(&self) -> u8 {
        ((self.inner >> 20) & 0b11111) as u8
    }
    pub fn rs3(&self) -> u8 {
        ((self.inner >> 27) & 0b11111) as u8
    }
    pub fn imm_i(&self) -> i32 {
        sign_extend((self.inner >> 20) as i32 & 0b111111111111, 12)
    }
    pub fn imm_s(&self) -> i32 {
        sign_extend(
            ((self.inner >> 20) & 0b111111100000) | 
            ((self.inner >> 7)  & 0b000000011111),
            12,
        )
    }
    pub fn imm_b(&self) -> i32 {
        sign_extend(
            ((self.inner >> 19) & 0b1000000000000) |
            ((self.inner << 4)  & 0b0100000000000) |
            ((self.inner >> 20) & 0b0011111100000) |
            ((self.inner >> 7)  & 0b0000000011110),
            13,
        )
    }
    pub fn imm_u(&self) -> i32 {
        self.inner & 0b11111111111111111111000000000000u32 as i32
    }
    pub fn imm_j(&self) -> i32 {
        sign_extend(
            ((self.inner >> 11) & 0b100000000000000000000) |
            ((self.inner >> 0)  & 0b011111111000000000000) |
            ((self.inner >> 9)  & 0b000000000100000000000) |
            ((self.inner >> 20) & 0b000000000011111111111),
            21,
        )
    }
    pub fn shamt(&self) -> u8 {
        self.rs2()
    }
    pub fn new(inner: i32, opcode: OpCode) -> Self {
        Instruction { inner, opcode }
    }
    pub fn execute(&self, state: &mut ProgramState) {
        use self::OpCode::*;
        let mut next_pc = state.regs.pc + 4;
        match self.opcode {
            ADDI => {
                let res = self.imm_i() + state.regs.read_x(self.rs1());
                state.regs.write_x(self.rd(), res);
            }
            ANDI => {
                let res = self.imm_i() & state.regs.read_x(self.rs1());
                state.regs.write_x(self.rd(), res);
            }
            ORI => {
                let res = self.imm_i() | state.regs.read_x(self.rs1());
                state.regs.write_x(self.rd(), res);
            }
            XORI => {
                let res = self.imm_i() ^ state.regs.read_x(self.rs1());
                state.regs.write_x(self.rd(), res);
            }
            SLTI => {
                let res = if state.regs.read_x(self.rs1()) < self.imm_i() {
                    1
                } else {
                    0
                };
                state.regs.write_x(self.rd(), res);
            }
            SLTIU => {
                let res = if (state.regs.read_x(self.rs1()) as u32) < (self.imm_i() as u32) {
                    1
                } else {
                    0
                };
                state.regs.write_x(self.rd(), res);
            }
            SRLI => {
                let res = state.regs.read_x(self.rs1()) >> self.rs2();
                state.regs.write_x(self.rd(), res);
            }
            SLLI => {
                let res = state.regs.read_x(self.rs1()) << self.rs2();
                state.regs.write_x(self.rd(), res);
            }
            SRAI => {
                let shamt = self.rs2() as u8;
                let res = state.regs.read_x(self.rs1()) >> shamt;
                let res = sign_extend(res, 32 - shamt);
                state.regs.write_x(self.rd(), res);
            }
            SB => {
                let addr = state.regs.read_x(self.rs1()) + self.imm_s();
                let val = state.regs.read_x(self.rs2()) as u8;
                state.mem.data[addr as usize] = val;
            }
            SH => {
                let addr = state.regs.read_x(self.rs1()) + self.imm_s();
                let val = state.regs.read_x(self.rs2()) as i16;
                LittleEndian::write_i16(
                    &mut state.mem.data[addr as usize..(addr + 2) as usize],
                    val,
                );
            }
            SW => {
                let addr = state.regs.read_x(self.rs1()) + self.imm_s();
                let val = state.regs.read_x(self.rs2());
                LittleEndian::write_i32(
                    &mut state.mem.data[addr as usize..(addr + 4) as usize],
                    val,
                );
            }
            LB => {
                let addr = state.regs.read_x(self.rs1()) + self.imm_i();
                let val = state.mem.data[addr as usize] as i32;
                state.regs.write_x(self.rd(), val);
            }
            LH => {
                let addr = state.regs.read_x(self.rs1()) + self.imm_i();
                let val =
                    LittleEndian::read_i16(&state.mem.data[addr as usize..(addr + 2) as usize]) as i32;
                state.regs.write_x(self.rd(), val);
            }
            LW => {
                let addr = state.regs.read_x(self.rs1()) + self.imm_i();
                let val =
                    LittleEndian::read_i32(&state.mem.data[addr as usize..(addr + 4) as usize]);
                state.regs.write_x(self.rd(), val);
            }
            ADD => {
                let res =
                    i32::wrapping_add(state.regs.read_x(self.rs1()), state.regs.read_x(self.rs2()));
                state.regs.write_x(self.rd(), res);
            }
            SUB => {
                let res = i32::wrapping_sub(self.imm_i(), state.regs.read_x(self.rs1()));
                state.regs.write_x(self.rd(), res);
            }
            AND => {
                let res =
                    state.regs.read_x(self.rs1()) & state.regs.read_x(self.rs2());
                state.regs.write_x(self.rd(), res);
            }
            OR => {
                let res =
                    state.regs.read_x(self.rs1()) | state.regs.read_x(self.rs2());
                state.regs.write_x(self.rd(), res);
            }
            SLL => {
                let res = self.imm_i() << state.regs.read_x(self.rs1());
                state.regs.write_x(self.rd(), res);
            }
            SRL => {
                let res = self.imm_i() >> state.regs.read_x(self.rs1());
                state.regs.write_x(self.rd(), res);
            }
            SRA => {
                let shamt = state.regs.read_x(self.rs2()) as u8;
                let res = state.regs.read_x(self.rs1()) >> shamt;
                let res = sign_extend(res, 32 - shamt);
                state.regs.write_x(self.rd(), res);
            }
            JAL => {
                let target = state.regs.pc + (self.imm_j() & !0b1);
                state.regs.write_x(self.rd(), next_pc);
                next_pc = target;
            }
            JALR => {
                let target = state.regs.read_x(self.rs1()) + self.imm_i();
                let target = target & !0b1;
                state.regs.write_x(self.rd(), next_pc);
                next_pc = target;
            }
            BNE => {
                if state.regs.read_x(self.rs1()) != state.regs.read_x(self.rs2()) {
                    next_pc = state.regs.pc + (self.imm_b() & !0b1);
                }
            }
            BEQ => {
                if state.regs.read_x(self.rs1()) == state.regs.read_x(self.rs2()) {
                    next_pc = state.regs.pc + (self.imm_b() & !0b1);
                }
            }
            BLT => {
                if state.regs.read_x(self.rs1()) < state.regs.read_x(self.rs2()) {
                    next_pc = state.regs.pc + (self.imm_b() & !0b1);
                }
            }
            BLTU => {
                if (state.regs.read_x(self.rs1()) as u32) < (state.regs.read_x(self.rs2()) as u32) {
                    next_pc = state.regs.pc + (self.imm_b() & !0b1);
                }
            }
            BGE => {
                if state.regs.read_x(self.rs1()) >= state.regs.read_x(self.rs2()) {
                    next_pc = state.regs.pc + (self.imm_b() & !0b1);
                }
            }
            BGEU => {
                if (state.regs.read_x(self.rs1()) as u32) >= (state.regs.read_x(self.rs2()) as u32) {
                    next_pc = state.regs.pc + (self.imm_b() & !0b1);
                }
            }
            MUL => {
                let res =
                    i32::wrapping_mul(state.regs.read_x(self.rs1()), state.regs.read_x(self.rs2()));
                state.regs.write_x(self.rd(), res);
            }
        }
        state.regs.pc = next_pc;
    }
    fn major_opcode(&self) -> MajorOpCode {
        MajorOpCode::decode(self.inner)
    }
    fn encoding_type(&self) -> EncodingType {
        use self::MajorOpCode::*;
        use self::EncodingType::*;
        match self.major_opcode() {
            AMO | OP | OP_32 | OP_FP => R,
            LOAD | LOAD_FP | MISC_MEM | OP_IMM | OP_IMM_32 | JALR | SYSTEM => I,
            STORE | STORE_FP => S,
            BRANCH => B,
            AUIPC | LUI => U,
            JAL => J,
            MADD | MSUB | NMSUB | NMADD => R4,
            RESERVED | CUSTOM | RV128 | LONG => None,
        }
    }
}

fn sign_extend<I, O>(input: I, from_len: u8) -> O
where
    O: From<I>,
    I: Copy + ops::BitOr<Output = I> + Eq + From<i8> + ops::Shl<u8, Output = I>,
{
    O::from(if input | (I::from(1) << (from_len - 1)) != input {
        input
    } else {
        input | (I::from(-1i8) << from_len)
    })
}


impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self.encoding_type() {
            EncodingType::R => {
                write!(
                    f,
                    "{:?} x{:02} x{:02} x{:02}",
                    self.opcode,
                    self.rd(),
                    self.rs1(),
                    self.rs2()
                )
            }
            EncodingType::I => {
                write!(
                    f,
                    "{:?} x{:02} x{:02} 0x{:03x}",
                    self.opcode,
                    self.rd(),
                    self.rs1(),
                    self.imm_i()
                )
            }
            EncodingType::S => {
                write!(
                    f,
                    "{:?} x{:02} x{:02} 0x{:03x}",
                    self.opcode,
                    self.rs1(),
                    self.rs2(),
                    self.imm_s()
                )
            }
            EncodingType::B => {
                write!(
                    f,
                    "{:?} x{:02} x{:02} 0x{:03x}",
                    self.opcode,
                    self.rs1(),
                    self.rs2(),
                    self.imm_b()
                )
            }
            EncodingType::U => {
                write!(
                    f,
                    "{:?} x{:02} 0x{:08x}",
                    self.opcode,
                    self.rd(),
                    self.imm_u()
                )
            }
            EncodingType::J => {
                write!(
                    f,
                    "{:?} x{:02} 0x{:08x}",
                    self.opcode,
                    self.rd(),
                    self.imm_j()
                )
            }
            EncodingType::R4 => {
                write!(
                    f,
                    "{:?} x{:02} x{:02} x{:02}, x{:02}, {:02b}",
                    self.opcode,
                    self.rd(),
                    self.rs1(),
                    self.rs2(),
                    self.rs3(),
                    self.funct2(),
                )
            }
            EncodingType::None => {
                Err(fmt::Error{})
            }
        }?;
        Ok(())
    }
}

#[derive(Debug, Clone, Copy)]
enum MajorOpCode {
    LOAD,
    LOAD_FP,
    MISC_MEM,
    OP_IMM,
    AUIPC,
    OP_IMM_32,
    STORE,
    STORE_FP,
    AMO,
    OP,
    LUI,
    OP_32,
    MADD,
    MSUB,
    NMSUB,
    NMADD,
    OP_FP,
    BRANCH,
    JALR,
    JAL,
    SYSTEM,

    RESERVED,
    CUSTOM,
    RV128,
    LONG,
}

impl MajorOpCode {
    fn decode(inst: i32) -> Self {
        use self::MajorOpCode::*;
        const MAJOR_OPCODE_TABLE: [MajorOpCode; 32] = [
            LOAD,
            LOAD_FP,
            CUSTOM,
            MISC_MEM,
            OP_IMM,
            AUIPC,
            OP_IMM_32,
            LONG,
            STORE,
            STORE_FP,
            CUSTOM,
            AMO,
            OP,
            LUI,
            OP_32,
            LONG,
            MADD,
            MSUB,
            NMSUB,
            NMADD,
            OP_FP,
            RESERVED,
            RV128,
            LONG,
            BRANCH,
            JALR,
            RESERVED,
            JAL,
            SYSTEM,
            RESERVED,
            RV128,
            LONG,
        ];
        let bits = (inst >> 2) & 0b11111;
        MAJOR_OPCODE_TABLE[bits as usize]
    }
}
pub fn decode(inst: i32) -> Instruction {
    let major_opcode = MajorOpCode::decode(inst);
    use self::MajorOpCode::*;
    let opcode = match major_opcode {
        OP_IMM => decode_op_imm(inst),
        OP => decode_op(inst),
        STORE => decode_store(inst),
        LOAD => decode_load(inst),
        JALR => decode_jalr(inst),
        BRANCH => decode_branch(inst),
        RESERVED => panic!("Unsupported Major Opcode ( reserved )"),
        CUSTOM => panic!("Unsupported Major Opcode ( custom )"),
        RV128 => panic!("Unsupported Major Opcode ( custom/RV128 )"),
        LONG => panic!("Unsupported Instruction Length ( > 32b )"),
        c @ _ => panic!("Unsupported Major Opcode ( {:?} )", c),
    };
    Instruction::new(inst, opcode)
}

fn decode_op(inst: i32) -> OpCode {
    use self::OpCode::*;
    let func3 = (inst >> 12) & 0b111;
    match func3 {
        0b000 => {
            let func7 = (inst >> 25) & 0b1111111;
            match func7 {
                0b0000000 => ADD,
                0b0000001 => MUL,
                0b0100000 => SUB,
                i @ _ => panic!("Unsupported instruction (OP-{:03b}-{:07b})", func3, i),
            }
        }
        0b101 => {
            let func7 = (inst >> 25) & 0b1111111;
            match func7 {
                0b0000000 => SRL,
                0b0100000 => SRA,
                i @ _ => panic!("Unsupported instruction (OP-{:03b}-{:07b})", func3, i),
            }
        }
        0b110 => {
            let func7 = (inst >> 25) & 0b1111111;
            match func7 {
                0b0000000 => OR,
                i @ _ => panic!("Unsupported instruction (OP-{:03b}-{:07b})", func3, i),
            }
        }
        0b111 => {
            let func7 = (inst >> 25) & 0b1111111;
            match func7 {
                0b0000000 => AND,
                i @ _ => panic!("Unsupported instruction (OP-{:03b}-{:07b})", func3, i),
            }
        }
        i @ _ => panic!("Unsupported instruction ( OP-{:03b} )", i),
    }
}

fn decode_store(inst: i32) -> OpCode {
    use self::OpCode::*;
    let func3 = (inst >> 12) & 0b111;
    match func3 {
        0b010 => SW,
        i @ _ => panic!("Unsupported instruction ( STORE-{:03b} )", i),
    }
}

fn decode_load(inst: i32) -> OpCode {
    use self::OpCode::*;
    let func3 = (inst >> 12) & 0b111;
    match func3 {
        0b000 => LB,
        0b001 => LH,
        0b010 => LW,
        i @ _ => panic!("Unsupported instruction ( LOAD-{:03b} )", i),
    }
}

fn decode_op_imm(inst: i32) -> OpCode {
    use self::OpCode::*;
    let func3 = (inst >> 12) & 0b111;
    match func3 {
        0b000 => ADDI,
        0b001 => {
            let func7 = (inst >> 25) & 0b1111111;
            match func7 {
                0b0000000 => SLLI,
                i @ _ => panic!("Unsupported instruction (OP_IMM-{:03b}-{:07b})", func3, i),
            }
        }
        0b010 => SLTI,
        0b011 => SLTIU,
        0b101 => {
            let func7 = (inst >> 25) & 0b1111111;
            match func7 {
                0b0000000 => SRLI,
                0b0100000 => SRAI,
                i @ _ => panic!("Unsupported instruction (OP_IMM-{:03b}-{:07b})", func3, i),
            }
        }
        0b110 => ORI,
        0b111 => ANDI,
        i @ _ => panic!("Unsupported instruction ( OP_IMM-{:03b} )", i),
    }
}

fn decode_jalr(inst: i32) -> OpCode {
    use self::OpCode::*;
    let func3 = (inst >> 12) & 0b111;
    match func3 {
        0b000 => JALR,
        i @ _ => panic!("Unsupported instruction ( JALR-{:03b} )", i),
    }
}

fn decode_branch(inst: i32) -> OpCode {
    use self::OpCode::*;
    let func3 = (inst >> 12) & 0b111;
    match func3 {
        0b000 => BEQ,
        0b001 => BNE,
        0b100 => BLT,
        0b101 => BGE,
        0b110 => BLTU,
        0b111 => BGEU,
        i @ _ => panic!("Unsupported instruction ( BRANCH-{:03b} )", i),
    }
}

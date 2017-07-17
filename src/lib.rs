#![allow(non_camel_case_types)]

use std::fmt;
extern crate byteorder;

pub mod instruction;

#[derive(Debug, Clone)]
pub struct Memory {
    pub data: Vec<u8>,
}

impl fmt::LowerHex for Memory {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let bytes_per_line: usize = 32;
        println!("");
        let mut printing = true;
        for (addr, chunk) in self.data.chunks(bytes_per_line).enumerate() {
            if chunk.iter().any(|b| *b != 0) {
                printing = true;
                write!(f, "{:08x} ", addr * bytes_per_line)?;
                for byte in chunk.iter() {
                    write!(f, "{:02x} ", byte)?;
                }
                writeln!(f, "")?;
            } else if printing {
                printing = false;
                writeln!(f, "         [...]")?;
            }
        }
        Ok(())
    }
}

impl Memory {
    pub fn new(size: usize) -> Self {
        Memory { data: vec![0; size] }
    }
    pub fn copy_segment(&mut self, segment: &[u8], offset: usize) -> Result<(), ()> {
        if offset + segment.len() > self.data.len() {
            return Err(());
        }
        self.data[offset..(offset + segment.len())].copy_from_slice(segment);
        Ok(())
    }
}

#[derive(Debug, Copy, Clone, Default)]
pub struct Registers {
    pub pc: i32,
    x: [i32; 32],
}

impl Registers {
    pub fn new() -> Self {
        Registers { pc: 0, x: [0; 32] }
    }
    pub fn read_x(&self, reg: u8) -> i32 {
        if reg == 0 { 0 } else { self.x[reg as usize] }
    }
    pub fn write_x(&mut self, reg: u8, val: i32) {
        if reg != 0 {
            self.x[reg as usize] = val;
        }
    }
}
impl fmt::Display for Registers {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        const REGISTER_NAMES: [&str; 32] = [
            "0",
            "ra",
            "sp",
            "gp",
            "tp",
            "t0",
            "t1",
            "t2",
            "s0",
            "s1",
            "a0",
            "a1",
            "a2",
            "a3",
            "a4",
            "a5",
            "a6",
            "a7",
            "s2",
            "s3",
            "s4",
            "s5",
            "s6",
            "s7",
            "s8",
            "s9",
            "s10",
            "s11",
            "t3",
            "t4",
            "t5",
            "t6",
        ];
        writeln!(f, "Program Counter: 0x{:08x}", self.pc)?;
        for i in 0..32 {
            let n = (i % 4) * 8 + i / 4;
            let name = REGISTER_NAMES[n as usize];
            write!(f, "{:3}:0x{:08x}  ", name, self.read_x(n))?;
            if i % 4 == 3 {
                writeln!(f)?;
            };
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct ProgramState {
    pub mem: Memory,
    pub regs: Registers,
}

impl fmt::Display for ProgramState {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "Memory map: {:x}", self.mem)?;
        write!(f, "{}", self.regs)?;
        Ok(())
    }
}

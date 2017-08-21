extern crate goblin;
extern crate scroll;
extern crate raik;
extern crate byteorder;

#[macro_use]
extern crate clap;

use std::fs::File;
use std::io;
use std::fmt::Display;
use scroll::{Buffer, Pread};
use byteorder::{LittleEndian, ByteOrder};
use raik::{Memory, Registers, ProgramState, instruction};

pub fn main() {

    let matches = clap_app!(rustv =>
        (version: crate_version!())
        (about: crate_description!())
        (@arg FILE: +required "Path to ELF file")
        (@arg debug: -d --debug "Enable debug mode")
    ).get_matches();

    let filename = matches.value_of("FILE").unwrap();
    let debug = matches.is_present("debug");

    let fd = File::open(filename).unwrap_or_else(|e| {
        exit_err(format!("Error opening {}: {}", filename, e));
    });
    let buffer = Buffer::try_from(fd).unwrap_or_else(|e| {
        exit_err(format!("Buffer error: {}", e));
    });
    let obj = goblin::parse(&buffer).unwrap_or_else(|e| {
        exit_err(format!("Error parsing {}: {}", filename, e));
    });

    let elf = match obj {
        goblin::Object::Elf(e) => e,
        _ => exit_err(format!("Error: {} does not appear to be an ELF file", filename))
    };

    if elf.header.e_machine != 0xf3 {
        exit_err(format!("Error: {} is not a Risc-V binary", filename));
    }

    if elf.is_64 {
        exit_err(format!("Error: {} is not a 32-bit binary", filename));
    }

    println!("Found valid RV32 ELF.");

    let mut memory = Memory::new(0x20000); // ought to be enough for anybody

    for ph in &elf.program_headers {
        match ph.p_type {
            0 | 4 | 5 | 6 => {}
            1 => {
                let segment = buffer
                    .pread_slice::<[u8]>(ph.p_offset as usize, ph.p_filesz as usize)
                    .unwrap();
                memory.copy_segment(segment, ph.p_vaddr as usize).unwrap();
            },
            2 => exit_err("Dynamic Linking is not supported!"),
            3 => exit_err("Error: Program Interpreters are not supported!"),
            t => exit_err(format!("Error: Unrecognized p_type: {}", t)),
        };
    }

    let mut regs = Registers::new();
    regs.pc = elf.entry as i32;
    let mut state = ProgramState {
        mem: memory,
        regs: regs,
    };
    let return_address = 0x42;

    state.regs.write_x(1, return_address as i32);
    state.regs.write_x(2, (state.mem.data.len() - 4) as i32);

    println!("Running now...");
    loop {
        let pc = state.regs.pc;
        let inst: i32 = LittleEndian::read_i32(&state.mem.data[pc as usize..(pc + 4) as usize]);
        let inst = instruction::decode(inst);
        println!("{}", state);
        println!("Next instruction: {}", inst);
        if debug {
            io::stdin().read_line(&mut String::new()).unwrap();
        }
        inst.execute(&mut state);
        if state.regs.pc == return_address {
            println!("Program returned with value {}", state.regs.read_x(10));
            std::process::exit(0);
        }
    }
}

fn exit_err<S : Display>(err : S) -> ! {
    eprintln!("{}", err);
    std::process::exit(1);
}

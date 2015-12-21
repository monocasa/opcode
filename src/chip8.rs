use super::{Addr, AddrTarget, Disassembler, DisError, DisResult};

#[derive(Debug, PartialEq)]
pub enum Reg {
	V(u8),
	I,
	Dt,
	K,
	St,
	F,
	B,
}

#[derive(Debug, PartialEq)]
pub enum Mne {
	Add,
	And,
	Call,
	Cls,
	Drw,
	Jp,
	Ld,
	Or,
	Ret,
	Rnd,
	Se,
	Shl,
	Shr,
	Sknp,
	Skp,
	Sne,
	Sub,
	Subn,
	Sys,
	Xor,
}

#[derive(Debug, PartialEq)]
pub enum Op {
	Implied(Mne),
	Addr(Mne, AddrTarget),
	RegImmU8(Mne, Reg, u8),
	RegReg(Mne, Reg, Reg),
	RegSorta(Mne, Reg, Reg),
	RegAddr(Mne, Reg, AddrTarget),
	RegRegImmU4(Mne, Reg, Reg, u8),
	Reg(Mne, Reg),
	IndirectReg(Mne, Reg),
	RegIndirect(Mne, Reg),
}

pub type DecodeResult = Result<Op, DisError>;

fn target_of(instr: u16) -> AddrTarget {
	AddrTarget::Absolute((instr & 0x0FFF) as u64)
}

fn vx_of(instr: u16) -> Reg {
	Reg::V(((instr & 0x0F00) >> 8) as u8)
}

fn vy_of(instr: u16) -> Reg {
	Reg::V(((instr & 0x00F0) >> 4) as u8)
}

fn kk_of(instr: u16) -> u8 {
	instr as u8
}

fn k_of(instr: u16) -> u8 {
	(instr & 0x000F) as u8
}

fn reg_to_str(reg: Reg) -> String {
	let ret = match reg {
		Reg::V(num) => return format!("v{:x}", num),
		Reg::I      => "i",
		Reg::Dt     => "dt",
		Reg::K      => "k",
		Reg::St     => "st",
		Reg::F      => "f",
		Reg::B      => "b",
	};

	ret.to_string()
}

fn mne_to_str(mne: Mne) -> String {
	let ret = match mne {
		Mne::Add  => "add",
		Mne::And  => "and",
		Mne::Call => "call",
		Mne::Cls  => "cls",
		Mne::Drw  => "drw",
		Mne::Jp   => "jp",
		Mne::Ld   => "ld",
		Mne::Or   => "or",
		Mne::Ret  => "ret",
		Mne::Rnd  => "rnd",
		Mne::Se   => "se",
		Mne::Shl  => "shl",
		Mne::Shr  => "shr",
		Mne::Sknp => "sknp",
		Mne::Skp  => "skp",
		Mne::Sne  => "sne",
		Mne::Sub  => "sub",
		Mne::Subn => "subn",
		Mne::Sys  => "sys",
		Mne::Xor  => "xor",
	};

	ret.to_string()
}

fn imm_to_str(imm: u8) -> String {
	if imm <= 9 {
		format!("{}", imm)
	} else {
		format!("{:#x}", imm)
	}
}

fn target_to_str(target: &AddrTarget) -> String {
	match target {
		&AddrTarget::Absolute(addr) => format!("{:#05x}", addr),
		&AddrTarget::Relative(offset) => format!("{:+}", offset),
		&AddrTarget::Symbol(ref symbol) => format!("{}", symbol),
	}
}

pub fn op_to_str(op: Op) -> String {
	match op {
		Op::Implied(mne)                    => mne_to_str(mne),
		Op::Addr(mne, target)               => format!("{:<4} {}", mne_to_str(mne), target_to_str(&target)),
		Op::RegImmU8(mne, dst, imm)         => format!("{:<4} {}, {}", mne_to_str(mne), reg_to_str(dst), imm_to_str(imm)),
		Op::RegReg(mne, dst, src)           => format!("{:<4} {}, {}", mne_to_str(mne), reg_to_str(dst), reg_to_str(src)),
		Op::RegSorta(mne, dst, ext)         => format!("{:<4} {} {{, {}}}", mne_to_str(mne), reg_to_str(dst), reg_to_str(ext)),
		Op::RegAddr(mne, reg, target)       => format!("{:<4} {}, {}", mne_to_str(mne), reg_to_str(reg), target_to_str(&target)),
		Op::RegRegImmU4(mne, dst, src, imm) => format!("{:<4} {}, {}, {}", mne_to_str(mne), reg_to_str(dst), reg_to_str(src), imm_to_str(imm)),
		Op::Reg(mne, reg)                   => format!("{:<4} {}", mne_to_str(mne), reg_to_str(reg)),
		Op::IndirectReg(mne, reg)           => format!("{:<4} [i], {}", mne_to_str(mne), reg_to_str(reg)),
		Op::RegIndirect(mne, reg)           => format!("{:<4} {}, [i]", mne_to_str(mne), reg_to_str(reg)),
	}
}

pub fn decode(instr: u16) -> DecodeResult {
	let op = match instr >> 12 {
		0x0 => match instr {
			0x00E0 => Op::Implied(Mne::Cls),
			0x00EE => Op::Implied(Mne::Ret),
			_      => Op::Addr(Mne::Sys, target_of(instr)),
		},

		0x1 => Op::Addr(Mne::Jp, target_of(instr)),

		0x2 => Op::Addr(Mne::Call, target_of(instr)),

		0x3 => Op::RegImmU8(Mne::Se, vx_of(instr), kk_of(instr)),

		0x4 => Op::RegImmU8(Mne::Sne, vx_of(instr), kk_of(instr)),

		0x5 => match instr & 0x000F {
			0x0 => Op::RegReg(Mne::Se, vx_of(instr), vy_of(instr)),
			_   => return Err(DisError::Unknown{num_bytes: 2}),
		},

		0x6 => Op::RegImmU8(Mne::Ld, vx_of(instr), kk_of(instr)),

		0x7 => Op::RegImmU8(Mne::Add, vx_of(instr), kk_of(instr)),

		0x8 => match instr & 0x000F {
			0x0 => Op::RegReg(  Mne::Ld,   vx_of(instr), vy_of(instr)),
			0x1 => Op::RegReg(  Mne::Or,   vx_of(instr), vy_of(instr)),
			0x2 => Op::RegReg(  Mne::And,  vx_of(instr), vy_of(instr)),
			0x3 => Op::RegReg(  Mne::Xor,  vx_of(instr), vy_of(instr)),
			0x4 => Op::RegReg(  Mne::Add,  vx_of(instr), vy_of(instr)),
			0x5 => Op::RegReg(  Mne::Sub,  vx_of(instr), vy_of(instr)),
			0x6 => Op::RegSorta(Mne::Shr,  vx_of(instr), vy_of(instr)),
			0x7 => Op::RegReg(  Mne::Subn, vx_of(instr), vy_of(instr)),
			0xE => Op::RegSorta(Mne::Shl,  vx_of(instr), vy_of(instr)),
			_   => return Err(DisError::Unknown{num_bytes: 2}),
		},

		0x9 => match instr & 0x000F {
			0x0 => Op::RegReg(Mne::Sne, vx_of(instr), vy_of(instr)),
			_   => return Err(DisError::Unknown{num_bytes: 2}),
		},

		0xA => Op::RegAddr(Mne::Ld, Reg::I, target_of(instr)),

		0xB => Op::RegAddr(Mne::Jp, Reg::V(0), target_of(instr)),

		0xC => Op::RegImmU8(Mne::Rnd, vx_of(instr), kk_of(instr)),

		0xD => Op::RegRegImmU4(Mne::Drw, vx_of(instr), vy_of(instr), k_of(instr)),

		0xE => match kk_of(instr) {
			0x9E => Op::Reg(Mne::Skp,  vx_of(instr)),
			0xA1 => Op::Reg(Mne::Sknp, vx_of(instr)),
			_    => return Err(DisError::Unknown{num_bytes: 2}),
		},

		0xF => match kk_of(instr) {
			0x07 => Op::RegReg(Mne::Ld,  vx_of(instr), Reg::Dt),
			0x0A => Op::RegReg(Mne::Ld,  vx_of(instr), Reg::K),
			0x15 => Op::RegReg(Mne::Ld,  Reg::Dt,      vx_of(instr)),
			0x18 => Op::RegReg(Mne::Ld,  Reg::St,      vx_of(instr)),
			0x1E => Op::RegReg(Mne::Add, Reg::I,       vx_of(instr)),
			0x29 => Op::RegReg(Mne::Ld,  Reg::F,       vx_of(instr)),
			0x33 => Op::RegReg(Mne::Ld,  Reg::B,       vx_of(instr)),
			0x55 => Op::IndirectReg(Mne::Ld, vx_of(instr)),
			0x65 => Op::RegIndirect(Mne::Ld, vx_of(instr)),
			_    => return Err(DisError::Unknown{num_bytes: 2}),
		},

		_ => return Err(DisError::Unknown{num_bytes: 2}),
	};

	Ok(op)
}

pub struct Chip8Disasm;

impl Disassembler for Chip8Disasm {
	fn disassemble(&self, addr: Addr, buf: &[u8]) -> DisResult {
		if addr & 1 != 0 {
			return Err(DisError::Unaligned{desired_alignment: 2});
		}

		if addr >= 0x0FFE {
			return Err(DisError::AddrOutOfRange{max_addr: 0x0FFE});
		}

		if buf.len() < 2 {
			return Err(DisError::MemOverflow);
		}

		let instr: u16 = ((buf[0] as u16) << 8) | (buf[1] as u16);

		let op = try!(decode(instr));

		Ok((op_to_str(op), 2, false))
	}

	fn bytes_per_unit(&self) -> u16 {
		2
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use super::super::{AddrTarget, Disassembler, DisError};

	struct TestCase {
		instr: u16,
		asm: &'static str,
		op: Op,
	}

	static TEST_CASES: [TestCase; 86] = [
		TestCase{ instr: 0x70FF, asm: "add  v0, 0xff",    op: Op::RegImmU8(Mne::Add, Reg::V(0x0), 0xFF)                    },
		TestCase{ instr: 0x7F00, asm: "add  vf, 0",       op: Op::RegImmU8(Mne::Add, Reg::V(0xF), 0x00)                    },
		TestCase{ instr: 0x80F4, asm: "add  v0, vf",      op: Op::RegReg(Mne::Add, Reg::V(0x0), Reg::V(0xF))               },
		TestCase{ instr: 0x8F04, asm: "add  vf, v0",      op: Op::RegReg(Mne::Add, Reg::V(0xF), Reg::V(0x0))               },
		TestCase{ instr: 0xF01E, asm: "add  i, v0",       op: Op::RegReg(Mne::Add, Reg::I, Reg::V(0x0))                    },
		TestCase{ instr: 0xFF1E, asm: "add  i, vf",       op: Op::RegReg(Mne::Add, Reg::I, Reg::V(0xF))                    },

		TestCase{ instr: 0x80F2, asm: "and  v0, vf",      op: Op::RegReg(Mne::And, Reg::V(0x0), Reg::V(0xF))               },
		TestCase{ instr: 0x8F02, asm: "and  vf, v0",      op: Op::RegReg(Mne::And, Reg::V(0xF), Reg::V(0x0))               },

		TestCase{ instr: 0x2000, asm: "call 0x000",       op: Op::Addr(Mne::Call, AddrTarget::Absolute(0x000))             },
		TestCase{ instr: 0x2423, asm: "call 0x423",       op: Op::Addr(Mne::Call, AddrTarget::Absolute(0x423))             },
		TestCase{ instr: 0x245C, asm: "call 0x45c",       op: Op::Addr(Mne::Call, AddrTarget::Absolute(0x45C))             },
		TestCase{ instr: 0x25E0, asm: "call 0x5e0",       op: Op::Addr(Mne::Call, AddrTarget::Absolute(0x5E0))             },
		TestCase{ instr: 0x27DB, asm: "call 0x7db",       op: Op::Addr(Mne::Call, AddrTarget::Absolute(0x7DB))             },
		TestCase{ instr: 0x2958, asm: "call 0x958",       op: Op::Addr(Mne::Call, AddrTarget::Absolute(0x958))             },
		TestCase{ instr: 0x2FFF, asm: "call 0xfff",       op: Op::Addr(Mne::Call, AddrTarget::Absolute(0xFFF))             },

		TestCase{ instr: 0x00E0, asm: "cls",              op: Op::Implied(Mne::Cls)                                        },

		TestCase{ instr: 0xD00F, asm: "drw  v0, v0, 0xf", op: Op::RegRegImmU4(Mne::Drw, Reg::V(0x0), Reg::V(0x0), 0xF)     },
		TestCase{ instr: 0xD0F0, asm: "drw  v0, vf, 0"  , op: Op::RegRegImmU4(Mne::Drw, Reg::V(0x0), Reg::V(0xF), 0x0)     },
		TestCase{ instr: 0xDF00, asm: "drw  vf, v0, 0",   op: Op::RegRegImmU4(Mne::Drw, Reg::V(0xF), Reg::V(0x0), 0x0)     },

		TestCase{ instr: 0x1000, asm: "jp   0x000",       op: Op::Addr(Mne::Jp, AddrTarget::Absolute(0x000))               },
		TestCase{ instr: 0x1423, asm: "jp   0x423",       op: Op::Addr(Mne::Jp, AddrTarget::Absolute(0x423))               },
		TestCase{ instr: 0x145C, asm: "jp   0x45c",       op: Op::Addr(Mne::Jp, AddrTarget::Absolute(0x45C))               },
		TestCase{ instr: 0x15E0, asm: "jp   0x5e0",       op: Op::Addr(Mne::Jp, AddrTarget::Absolute(0x5E0))               },
		TestCase{ instr: 0x17DB, asm: "jp   0x7db",       op: Op::Addr(Mne::Jp, AddrTarget::Absolute(0x7DB))               },
		TestCase{ instr: 0x1958, asm: "jp   0x958",       op: Op::Addr(Mne::Jp, AddrTarget::Absolute(0x958))               },
		TestCase{ instr: 0x1FFF, asm: "jp   0xfff",       op: Op::Addr(Mne::Jp, AddrTarget::Absolute(0xFFF))               },
		TestCase{ instr: 0xB000, asm: "jp   v0, 0x000",   op: Op::RegAddr(Mne::Jp, Reg::V(0), AddrTarget::Absolute(0x000)) },
		TestCase{ instr: 0xBD72, asm: "jp   v0, 0xd72",   op: Op::RegAddr(Mne::Jp, Reg::V(0), AddrTarget::Absolute(0xD72)) },
		TestCase{ instr: 0xBFFF, asm: "jp   v0, 0xfff",   op: Op::RegAddr(Mne::Jp, Reg::V(0), AddrTarget::Absolute(0xFFF)) },

		TestCase{ instr: 0x60FF, asm: "ld   v0, 0xff",    op: Op::RegImmU8(Mne::Ld, Reg::V(0x0), 0xFF)                     },
		TestCase{ instr: 0x6F00, asm: "ld   vf, 0",       op: Op::RegImmU8(Mne::Ld, Reg::V(0xF), 0x00)                     },
		TestCase{ instr: 0x80F0, asm: "ld   v0, vf",      op: Op::RegReg(Mne::Ld, Reg::V(0x0), Reg::V(0xF))                },
		TestCase{ instr: 0x8F00, asm: "ld   vf, v0",      op: Op::RegReg(Mne::Ld, Reg::V(0xF), Reg::V(0x0))                },
		TestCase{ instr: 0xA000, asm: "ld   i, 0x000",    op: Op::RegAddr(Mne::Ld, Reg::I, AddrTarget::Absolute(0x000))    },
		TestCase{ instr: 0xA69E, asm: "ld   i, 0x69e",    op: Op::RegAddr(Mne::Ld, Reg::I, AddrTarget::Absolute(0x69E))    },
		TestCase{ instr: 0xAFFF, asm: "ld   i, 0xfff",    op: Op::RegAddr(Mne::Ld, Reg::I, AddrTarget::Absolute(0xFFF))    },
		TestCase{ instr: 0xF007, asm: "ld   v0, dt",      op: Op::RegReg(Mne::Ld, Reg::V(0x0), Reg::Dt)                    },
		TestCase{ instr: 0xFF07, asm: "ld   vf, dt",      op: Op::RegReg(Mne::Ld, Reg::V(0xF), Reg::Dt)                    },
		TestCase{ instr: 0xF00A, asm: "ld   v0, k",       op: Op::RegReg(Mne::Ld, Reg::V(0x0), Reg::K)                     },
		TestCase{ instr: 0xFF0A, asm: "ld   vf, k",       op: Op::RegReg(Mne::Ld, Reg::V(0xF), Reg::K)                     },
		TestCase{ instr: 0xF015, asm: "ld   dt, v0",      op: Op::RegReg(Mne::Ld, Reg::Dt, Reg::V(0x0))                    },
		TestCase{ instr: 0xFF15, asm: "ld   dt, vf",      op: Op::RegReg(Mne::Ld, Reg::Dt, Reg::V(0xF))                    },
		TestCase{ instr: 0xF018, asm: "ld   st, v0",      op: Op::RegReg(Mne::Ld, Reg::St, Reg::V(0x0))                    },
		TestCase{ instr: 0xFF18, asm: "ld   st, vf",      op: Op::RegReg(Mne::Ld, Reg::St, Reg::V(0xF))                    },
		TestCase{ instr: 0xF029, asm: "ld   f, v0",       op: Op::RegReg(Mne::Ld, Reg::F, Reg::V(0x0))                     },
		TestCase{ instr: 0xFF29, asm: "ld   f, vf",       op: Op::RegReg(Mne::Ld, Reg::F, Reg::V(0xF))                     },
		TestCase{ instr: 0xF033, asm: "ld   b, v0",       op: Op::RegReg(Mne::Ld, Reg::B, Reg::V(0x0))                     },
		TestCase{ instr: 0xFF33, asm: "ld   b, vf",       op: Op::RegReg(Mne::Ld, Reg::B, Reg::V(0xF))                     },
		TestCase{ instr: 0xF055, asm: "ld   [i], v0",     op: Op::IndirectReg(Mne::Ld, Reg::V(0x0))                        },
		TestCase{ instr: 0xFF55, asm: "ld   [i], vf",     op: Op::IndirectReg(Mne::Ld, Reg::V(0xF))                        },
		TestCase{ instr: 0xF065, asm: "ld   v0, [i]",     op: Op::RegIndirect(Mne::Ld, Reg::V(0x0))                        },
		TestCase{ instr: 0xFF65, asm: "ld   vf, [i]",     op: Op::RegIndirect(Mne::Ld, Reg::V(0xF))                        },

		TestCase{ instr: 0x80F1, asm: "or   v0, vf",      op: Op::RegReg(Mne::Or, Reg::V(0x0), Reg::V(0xF))                },
		TestCase{ instr: 0x8F01, asm: "or   vf, v0",      op: Op::RegReg(Mne::Or, Reg::V(0xf), Reg::V(0x0))                },

		TestCase{ instr: 0x00EE, asm: "ret",              op: Op::Implied(Mne::Ret)                                        },

		TestCase{ instr: 0xC0FF, asm: "rnd  v0, 0xff",    op: Op::RegImmU8(Mne::Rnd, Reg::V(0x0), 0xFF)                    },
		TestCase{ instr: 0xCF00, asm: "rnd  vf, 0",       op: Op::RegImmU8(Mne::Rnd, Reg::V(0xF), 0x00)                    },

		TestCase{ instr: 0x3000, asm: "se   v0, 0",       op: Op::RegImmU8(Mne::Se, Reg::V(0x0), 0x00)                     },
		TestCase{ instr: 0x3567, asm: "se   v5, 0x67",    op: Op::RegImmU8(Mne::Se, Reg::V(0x5), 0x67)                     },
		TestCase{ instr: 0x3FFF, asm: "se   vf, 0xff",    op: Op::RegImmU8(Mne::Se, Reg::V(0xF), 0xFF)                     },
		TestCase{ instr: 0x50F0, asm: "se   v0, vf",      op: Op::RegReg(Mne::Se, Reg::V(0x0), Reg::V(0xF))                },
		TestCase{ instr: 0x5F00, asm: "se   vf, v0",      op: Op::RegReg(Mne::Se, Reg::V(0xF), Reg::V(0x0))                },

		TestCase{ instr: 0x80FE, asm: "shl  v0 {, vf}",   op: Op::RegSorta(Mne::Shl, Reg::V(0x0), Reg::V(0xF))             },
		TestCase{ instr: 0x8F0E, asm: "shl  vf {, v0}",   op: Op::RegSorta(Mne::Shl, Reg::V(0xF), Reg::V(0x0))             },

		TestCase{ instr: 0x80F6, asm: "shr  v0 {, vf}",   op: Op::RegSorta(Mne::Shr, Reg::V(0x0), Reg::V(0xF))             },
		TestCase{ instr: 0x8F06, asm: "shr  vf {, v0}",   op: Op::RegSorta(Mne::Shr, Reg::V(0xF), Reg::V(0x0))             },

		TestCase{ instr: 0xE0A1, asm: "sknp v0",          op: Op::Reg(Mne::Sknp, Reg::V(0x0))                              },
		TestCase{ instr: 0xEFA1, asm: "sknp vf",          op: Op::Reg(Mne::Sknp, Reg::V(0xF))                              },

		TestCase{ instr: 0xE09E, asm: "skp  v0",          op: Op::Reg(Mne::Skp, Reg::V(0x0))                               },
		TestCase{ instr: 0xEF9E, asm: "skp  vf",          op: Op::Reg(Mne::Skp, Reg::V(0xF))                               },

		TestCase{ instr: 0x4000, asm: "sne  v0, 0",       op: Op::RegImmU8(Mne::Sne, Reg::V(0x0), 0x00)                    },
		TestCase{ instr: 0x4A45, asm: "sne  va, 0x45",    op: Op::RegImmU8(Mne::Sne, Reg::V(0xA), 0x45)                    },
		TestCase{ instr: 0x4FFF, asm: "sne  vf, 0xff",    op: Op::RegImmU8(Mne::Sne, Reg::V(0xF), 0xFF)                    },
		TestCase{ instr: 0x90F0, asm: "sne  v0, vf",      op: Op::RegReg(Mne::Sne, Reg::V(0x0), Reg::V(0xF))               },
		TestCase{ instr: 0x9F00, asm: "sne  vf, v0",      op: Op::RegReg(Mne::Sne, Reg::V(0xF), Reg::V(0x0))               },

		TestCase{ instr: 0x80F5, asm: "sub  v0, vf",      op: Op::RegReg(Mne::Sub, Reg::V(0x0), Reg::V(0xF))               },
		TestCase{ instr: 0x8F05, asm: "sub  vf, v0",      op: Op::RegReg(Mne::Sub, Reg::V(0xF), Reg::V(0x0))               },

		TestCase{ instr: 0x80F7, asm: "subn v0, vf",      op: Op::RegReg(Mne::Subn, Reg::V(0x0), Reg::V(0xF))              },
		TestCase{ instr: 0x8F07, asm: "subn vf, v0",      op: Op::RegReg(Mne::Subn, Reg::V(0xF), Reg::V(0x0))              },

		TestCase{ instr: 0x0000, asm: "sys  0x000",       op: Op::Addr(Mne::Sys, AddrTarget::Absolute(0x000))              },
		TestCase{ instr: 0x0010, asm: "sys  0x010",       op: Op::Addr(Mne::Sys, AddrTarget::Absolute(0x010))              },
		TestCase{ instr: 0x00DF, asm: "sys  0x0df",       op: Op::Addr(Mne::Sys, AddrTarget::Absolute(0x0DF))              },
		TestCase{ instr: 0x00F0, asm: "sys  0x0f0",       op: Op::Addr(Mne::Sys, AddrTarget::Absolute(0x0F0))              },
		TestCase{ instr: 0x00FF, asm: "sys  0x0ff",       op: Op::Addr(Mne::Sys, AddrTarget::Absolute(0x0FF))              },

		TestCase{ instr: 0x80F3, asm: "xor  v0, vf",      op: Op::RegReg(Mne::Xor, Reg::V(0x0), Reg::V(0xF))               },
		TestCase{ instr: 0x8F03, asm: "xor  vf, v0",      op: Op::RegReg(Mne::Xor, Reg::V(0xF), Reg::V(0x0))               },
	];

	#[test]
	fn decode_instrs() {
		for test_case in TEST_CASES.iter() {
			assert_eq!(decode(test_case.instr).unwrap(), test_case.op);
		}
	}

	#[test]
	fn disasm_instrs() {
		let mut buffer: [u8;2] = [0, 0];
		let disasm: Box<Disassembler> = Box::new(Chip8Disasm);

		for test_case in TEST_CASES.iter() {
			buffer[0] = (test_case.instr >> 8) as u8;
			buffer[1] = test_case.instr as u8;

			assert_eq!(disasm.disassemble(0, &buffer), 
			           Ok((test_case.asm.to_string(), 2, false)));
		}
	}

	#[test]
	fn decode_unknown() {
		assert_eq!(decode(0x5001), Err(DisError::Unknown{num_bytes: 2}));
		assert_eq!(decode(0x5003), Err(DisError::Unknown{num_bytes: 2}));
		assert_eq!(decode(0x500F), Err(DisError::Unknown{num_bytes: 2}));

		assert_eq!(decode(0x8008), Err(DisError::Unknown{num_bytes: 2}));
		assert_eq!(decode(0x8009), Err(DisError::Unknown{num_bytes: 2}));
		assert_eq!(decode(0x800A), Err(DisError::Unknown{num_bytes: 2}));
		assert_eq!(decode(0x800B), Err(DisError::Unknown{num_bytes: 2}));
		assert_eq!(decode(0x800C), Err(DisError::Unknown{num_bytes: 2}));
		assert_eq!(decode(0x800D), Err(DisError::Unknown{num_bytes: 2}));
		assert_eq!(decode(0x800F), Err(DisError::Unknown{num_bytes: 2}));
	}
}


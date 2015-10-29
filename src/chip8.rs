use super::{Addr, Disassembler, DisError, DisResult};

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
	Addr(Mne, u16),
	RegImmU8(Mne, Reg, u8),
	RegReg(Mne, Reg, Reg),
	RegSorta(Mne, Reg, Reg),
	RegAddr(Mne, Reg, u16),
	RegRegImmU4(Mne, Reg, Reg, u8),
	Reg(Mne, Reg),
	IndirectReg(Mne, Reg),
	RegIndirect(Mne, Reg),
}

pub type DecodeResult = Result<Op, DisError>;

fn addr_of(instr: u16) -> u16 {
	instr & 0x0FFF
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

fn addr_to_str(addr: u16) -> String {
	format!("{:#05x}", addr)
}

pub fn op_to_str(op: Op) -> String {
	match op {
		Op::Implied(mne)                    => mne_to_str(mne),
		Op::Addr(mne, addr)                 => format!("{:<4} {}", mne_to_str(mne), addr_to_str(addr)),
		Op::RegImmU8(mne, dst, imm)         => format!("{:<4} {}, {}", mne_to_str(mne), reg_to_str(dst), imm_to_str(imm)),
		Op::RegReg(mne, dst, src)           => format!("{:<4} {}, {}", mne_to_str(mne), reg_to_str(dst), reg_to_str(src)),
		Op::RegSorta(mne, dst, ext)         => format!("{:<4} {} {{, {}}}", mne_to_str(mne), reg_to_str(dst), reg_to_str(ext)),
		Op::RegAddr(mne, reg, addr)         => format!("{:<4} {}, {}", mne_to_str(mne), reg_to_str(reg), addr_to_str(addr)),
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
			_      => Op::Addr(Mne::Sys, addr_of(instr)),
		},

		0x1 => Op::Addr(Mne::Jp, addr_of(instr)),

		0x2 => Op::Addr(Mne::Call, addr_of(instr)),

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

		0xA => Op::RegAddr(Mne::Ld, Reg::I, addr_of(instr)),

		0xB => Op::RegAddr(Mne::Jp, Reg::V(0), addr_of(instr)),

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

		Ok((op_to_str(op), 2))
	}

	fn bytes_per_unit(&self) -> u16 {
		2
	}

	fn typical_num_units(&self) -> u16 {
		1
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use super::super::DisError;

	#[test]
	fn decode_add() {
		assert_eq!(decode(0x70FF).unwrap(), Op::RegImmU8(Mne::Add, Reg::V(0x0), 0xFF));
		assert_eq!(decode(0x7F00).unwrap(), Op::RegImmU8(Mne::Add, Reg::V(0xF), 0x00));

		assert_eq!(decode(0x80F4).unwrap(), Op::RegReg(Mne::Add, Reg::V(0x0), Reg::V(0xF)));
		assert_eq!(decode(0x8F04).unwrap(), Op::RegReg(Mne::Add, Reg::V(0xF), Reg::V(0x0)));

		assert_eq!(decode(0xF01E).unwrap(), Op::RegReg(Mne::Add, Reg::I, Reg::V(0x0)));
		assert_eq!(decode(0xFF1E).unwrap(), Op::RegReg(Mne::Add, Reg::I, Reg::V(0xF)));
	}

	#[test]
	fn decode_and() {
		assert_eq!(decode(0x80F2).unwrap(), Op::RegReg(Mne::And, Reg::V(0x0), Reg::V(0xF)));
		assert_eq!(decode(0x8F02).unwrap(), Op::RegReg(Mne::And, Reg::V(0xF), Reg::V(0x0)));
	}

	#[test]
	fn decode_call() {
		assert_eq!(decode(0x2000).unwrap(), Op::Addr(Mne::Call, 0x000));
		assert_eq!(decode(0x2423).unwrap(), Op::Addr(Mne::Call, 0x423));
		assert_eq!(decode(0x245C).unwrap(), Op::Addr(Mne::Call, 0x45C));
		assert_eq!(decode(0x25E0).unwrap(), Op::Addr(Mne::Call, 0x5E0));
		assert_eq!(decode(0x27DB).unwrap(), Op::Addr(Mne::Call, 0x7DB));
		assert_eq!(decode(0x2958).unwrap(), Op::Addr(Mne::Call, 0x958));
		assert_eq!(decode(0x2FFF).unwrap(), Op::Addr(Mne::Call, 0xFFF));
	}

	#[test]
	fn decode_cls() {
		assert_eq!(decode(0x00E0).unwrap(), Op::Implied(Mne::Cls));
	}

	#[test]
	fn decode_drw() {
		assert_eq!(decode(0xD00F).unwrap(), Op::RegRegImmU4(Mne::Drw, Reg::V(0x0), Reg::V(0x0), 0xF));
		assert_eq!(decode(0xD0F0).unwrap(), Op::RegRegImmU4(Mne::Drw, Reg::V(0x0), Reg::V(0xF), 0x0));
		assert_eq!(decode(0xDF00).unwrap(), Op::RegRegImmU4(Mne::Drw, Reg::V(0xF), Reg::V(0x0), 0x0));
	}

	#[test]
	fn decode_jp() {
		assert_eq!(decode(0x1000).unwrap(), Op::Addr(Mne::Jp, 0x000));
		assert_eq!(decode(0x1423).unwrap(), Op::Addr(Mne::Jp, 0x423));
		assert_eq!(decode(0x145C).unwrap(), Op::Addr(Mne::Jp, 0x45C));
		assert_eq!(decode(0x15E0).unwrap(), Op::Addr(Mne::Jp, 0x5E0));
		assert_eq!(decode(0x17DB).unwrap(), Op::Addr(Mne::Jp, 0x7DB));
		assert_eq!(decode(0x1958).unwrap(), Op::Addr(Mne::Jp, 0x958));
		assert_eq!(decode(0x1FFF).unwrap(), Op::Addr(Mne::Jp, 0xFFF));

		assert_eq!(decode(0xB000).unwrap(), Op::RegAddr(Mne::Jp, Reg::V(0), 0x000));
		assert_eq!(decode(0xBD72).unwrap(), Op::RegAddr(Mne::Jp, Reg::V(0), 0xD72));
		assert_eq!(decode(0xBFFF).unwrap(), Op::RegAddr(Mne::Jp, Reg::V(0), 0xFFF));
	}

	#[test]
	fn decode_ld() {
		assert_eq!(decode(0x60FF).unwrap(), Op::RegImmU8(Mne::Ld, Reg::V(0x0), 0xFF));
		assert_eq!(decode(0x6F00).unwrap(), Op::RegImmU8(Mne::Ld, Reg::V(0xF), 0x00));

		assert_eq!(decode(0x80F0).unwrap(), Op::RegReg(Mne::Ld, Reg::V(0x0), Reg::V(0xF)));
		assert_eq!(decode(0x8F00).unwrap(), Op::RegReg(Mne::Ld, Reg::V(0xF), Reg::V(0x0)));

		assert_eq!(decode(0xA000).unwrap(), Op::RegAddr(Mne::Ld, Reg::I, 0x000));
		assert_eq!(decode(0xA69E).unwrap(), Op::RegAddr(Mne::Ld, Reg::I, 0x69E));
		assert_eq!(decode(0xAFFF).unwrap(), Op::RegAddr(Mne::Ld, Reg::I, 0xFFF));

		assert_eq!(decode(0xF007).unwrap(), Op::RegReg(Mne::Ld, Reg::V(0x0), Reg::Dt));
		assert_eq!(decode(0xFF07).unwrap(), Op::RegReg(Mne::Ld, Reg::V(0xF), Reg::Dt));

		assert_eq!(decode(0xF00A).unwrap(), Op::RegReg(Mne::Ld, Reg::V(0x0), Reg::K));
		assert_eq!(decode(0xFF0A).unwrap(), Op::RegReg(Mne::Ld, Reg::V(0xF), Reg::K));

		assert_eq!(decode(0xF015).unwrap(), Op::RegReg(Mne::Ld, Reg::Dt, Reg::V(0x0)));
		assert_eq!(decode(0xFF15).unwrap(), Op::RegReg(Mne::Ld, Reg::Dt, Reg::V(0xF)));

		assert_eq!(decode(0xF018).unwrap(), Op::RegReg(Mne::Ld, Reg::St, Reg::V(0x0)));
		assert_eq!(decode(0xFF18).unwrap(), Op::RegReg(Mne::Ld, Reg::St, Reg::V(0xF)));

		assert_eq!(decode(0xF029).unwrap(), Op::RegReg(Mne::Ld, Reg::F, Reg::V(0x0)));
		assert_eq!(decode(0xFF29).unwrap(), Op::RegReg(Mne::Ld, Reg::F, Reg::V(0xF)));

		assert_eq!(decode(0xF033).unwrap(), Op::RegReg(Mne::Ld, Reg::B, Reg::V(0x0)));
		assert_eq!(decode(0xFF33).unwrap(), Op::RegReg(Mne::Ld, Reg::B, Reg::V(0xF)));

		assert_eq!(decode(0xF055).unwrap(), Op::IndirectReg(Mne::Ld, Reg::V(0x0)));
		assert_eq!(decode(0xFF55).unwrap(), Op::IndirectReg(Mne::Ld, Reg::V(0xF)));

		assert_eq!(decode(0xF065).unwrap(), Op::RegIndirect(Mne::Ld, Reg::V(0x0)));
		assert_eq!(decode(0xFF65).unwrap(), Op::RegIndirect(Mne::Ld, Reg::V(0xF)));
	}

	#[test]
	fn decode_or() {
		assert_eq!(decode(0x80F1).unwrap(), Op::RegReg(Mne::Or, Reg::V(0x0), Reg::V(0xF)));
		assert_eq!(decode(0x8F01).unwrap(), Op::RegReg(Mne::Or, Reg::V(0xF), Reg::V(0x0)));
	}

	#[test]
	fn decode_ret() {
		assert_eq!(decode(0x00EE).unwrap(), Op::Implied(Mne::Ret));
	}

	#[test]
	fn decode_rnd() {
		assert_eq!(decode(0xC0FF).unwrap(), Op::RegImmU8(Mne::Rnd, Reg::V(0x0), 0xFF));
		assert_eq!(decode(0xCF00).unwrap(), Op::RegImmU8(Mne::Rnd, Reg::V(0xF), 0x00));
	}

	#[test]
	fn decode_se() {
		assert_eq!(decode(0x3000).unwrap(), Op::RegImmU8(Mne::Se, Reg::V(0x0), 0x00));
		assert_eq!(decode(0x3567).unwrap(), Op::RegImmU8(Mne::Se, Reg::V(0x5), 0x67));
		assert_eq!(decode(0x3FFF).unwrap(), Op::RegImmU8(Mne::Se, Reg::V(0xF), 0xFF));

		assert_eq!(decode(0x50F0).unwrap(), Op::RegReg(Mne::Se, Reg::V(0x0), Reg::V(0xF)));
		assert_eq!(decode(0x5F00).unwrap(), Op::RegReg(Mne::Se, Reg::V(0xF), Reg::V(0x0)));
	}

	#[test]
	fn decode_shl() {
		assert_eq!(decode(0x80FE).unwrap(), Op::RegSorta(Mne::Shl, Reg::V(0x0), Reg::V(0xF)));
		assert_eq!(decode(0x8F0E).unwrap(), Op::RegSorta(Mne::Shl, Reg::V(0xF), Reg::V(0x0)));
	}

	#[test]
	fn decode_shr() {
		assert_eq!(decode(0x80F6).unwrap(), Op::RegSorta(Mne::Shr, Reg::V(0x0), Reg::V(0xF)));
		assert_eq!(decode(0x8F06).unwrap(), Op::RegSorta(Mne::Shr, Reg::V(0xF), Reg::V(0x0)));
	}

	#[test]
	fn decode_sknp() {
		assert_eq!(decode(0xE0A1).unwrap(), Op::Reg(Mne::Sknp, Reg::V(0x0)));
		assert_eq!(decode(0xEFA1).unwrap(), Op::Reg(Mne::Sknp, Reg::V(0xF)));
	}

	#[test]
	fn decode_skp() {
		assert_eq!(decode(0xE09E).unwrap(), Op::Reg(Mne::Skp, Reg::V(0x0)));
		assert_eq!(decode(0xEF9E).unwrap(), Op::Reg(Mne::Skp, Reg::V(0xF)));
	}

	#[test]
	fn decode_sne() {
		assert_eq!(decode(0x4000).unwrap(), Op::RegImmU8(Mne::Sne, Reg::V(0), 0x00));
		assert_eq!(decode(0x4A45).unwrap(), Op::RegImmU8(Mne::Sne, Reg::V(0xA), 0x45));
		assert_eq!(decode(0x4FFF).unwrap(), Op::RegImmU8(Mne::Sne, Reg::V(0xF), 0xFF));

		assert_eq!(decode(0x90F0).unwrap(), Op::RegReg(Mne::Sne, Reg::V(0x0), Reg::V(0xF)));
		assert_eq!(decode(0x9F00).unwrap(), Op::RegReg(Mne::Sne, Reg::V(0xF), Reg::V(0x0)));
	}

	#[test]
	fn decode_sub() {
		assert_eq!(decode(0x80F5).unwrap(), Op::RegReg(Mne::Sub, Reg::V(0x0), Reg::V(0xF)));
		assert_eq!(decode(0x8F05).unwrap(), Op::RegReg(Mne::Sub, Reg::V(0xF), Reg::V(0x0)));
	}

	#[test]
	fn decode_subn() {
		assert_eq!(decode(0x80F7).unwrap(), Op::RegReg(Mne::Subn, Reg::V(0x0), Reg::V(0xF)));
		assert_eq!(decode(0x8F07).unwrap(), Op::RegReg(Mne::Subn, Reg::V(0xF), Reg::V(0x0)));
	}

	#[test]
	fn decode_sys() {
		assert_eq!(decode(0x0000).unwrap(), Op::Addr(Mne::Sys, 0x00));
		assert_eq!(decode(0x0010).unwrap(), Op::Addr(Mne::Sys, 0x10));
		assert_eq!(decode(0x00DF).unwrap(), Op::Addr(Mne::Sys, 0xDF));
		assert_eq!(decode(0x00F0).unwrap(), Op::Addr(Mne::Sys, 0xF0));
		assert_eq!(decode(0x00FF).unwrap(), Op::Addr(Mne::Sys, 0xFF));
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

	#[test]
	fn decode_xor() {
		assert_eq!(decode(0x80F3).unwrap(), Op::RegReg(Mne::Xor, Reg::V(0x0), Reg::V(0xF)));
		assert_eq!(decode(0x8F03).unwrap(), Op::RegReg(Mne::Xor, Reg::V(0xF), Reg::V(0x0)));
	}

	#[test]
	fn disasm_add() {
		assert_eq!(op_to_str(Op::RegImmU8(Mne::Add, Reg::V(0x0), 0xFF)), "add  v0, 0xff");
		assert_eq!(op_to_str(Op::RegImmU8(Mne::Add, Reg::V(0xF), 0x00)), "add  vf, 0");

		assert_eq!(op_to_str(Op::RegReg(Mne::Add, Reg::V(0x0), Reg::V(0xF))), "add  v0, vf");
		assert_eq!(op_to_str(Op::RegReg(Mne::Add, Reg::V(0xF), Reg::V(0x0))), "add  vf, v0");

		assert_eq!(op_to_str(Op::RegReg(Mne::Add, Reg::I, Reg::V(0x0))), "add  i, v0");
		assert_eq!(op_to_str(Op::RegReg(Mne::Add, Reg::I, Reg::V(0xF))), "add  i, vf");
	}

	#[test]
	fn disasm_and() {
		assert_eq!(op_to_str(Op::RegReg(Mne::And, Reg::V(0x0), Reg::V(0xF))), "and  v0, vf");
		assert_eq!(op_to_str(Op::RegReg(Mne::And, Reg::V(0xF), Reg::V(0x0))), "and  vf, v0");
	}

	#[test]
	fn disasm_call() {
		assert_eq!(op_to_str(Op::Addr(Mne::Call, 0x000)), "call 0x000");
		assert_eq!(op_to_str(Op::Addr(Mne::Call, 0x423)), "call 0x423");
		assert_eq!(op_to_str(Op::Addr(Mne::Call, 0x45C)), "call 0x45c");
		assert_eq!(op_to_str(Op::Addr(Mne::Call, 0x5E0)), "call 0x5e0");
		assert_eq!(op_to_str(Op::Addr(Mne::Call, 0x7DB)), "call 0x7db");
		assert_eq!(op_to_str(Op::Addr(Mne::Call, 0x958)), "call 0x958");
		assert_eq!(op_to_str(Op::Addr(Mne::Call, 0xFFF)), "call 0xfff");
	}

	#[test]
	fn disasm_cls() {
		assert_eq!(op_to_str(Op::Implied(Mne::Cls)), "cls");
	}

	#[test]
	fn disasm_drw() {
		assert_eq!(op_to_str(Op::RegRegImmU4(Mne::Drw, Reg::V(0x0), Reg::V(0x0), 0xF)), "drw  v0, v0, 0xf");
		assert_eq!(op_to_str(Op::RegRegImmU4(Mne::Drw, Reg::V(0x0), Reg::V(0xF), 0x0)), "drw  v0, vf, 0");
		assert_eq!(op_to_str(Op::RegRegImmU4(Mne::Drw, Reg::V(0xF), Reg::V(0x0), 0x0)), "drw  vf, v0, 0");
	}

	#[test]
	fn disasm_jp() {
		assert_eq!(op_to_str(Op::Addr(Mne::Jp, 0x000)), "jp   0x000");
		assert_eq!(op_to_str(Op::Addr(Mne::Jp, 0xFFF)), "jp   0xfff");

		assert_eq!(op_to_str(Op::RegAddr(Mne::Jp, Reg::V(0x0), 0x000)), "jp   v0, 0x000");
		assert_eq!(op_to_str(Op::RegAddr(Mne::Jp, Reg::V(0x4), 0xD72)), "jp   v4, 0xd72");
		assert_eq!(op_to_str(Op::RegAddr(Mne::Jp, Reg::V(0xF), 0xFFF)), "jp   vf, 0xfff");
	}

	#[test]
	fn disasm_ld() {
		assert_eq!(op_to_str(Op::RegImmU8(Mne::Ld, Reg::V(0x0), 0xFF)), "ld   v0, 0xff");
		assert_eq!(op_to_str(Op::RegImmU8(Mne::Ld, Reg::V(0xF), 0x00)), "ld   vf, 0");

		assert_eq!(op_to_str(Op::RegReg(Mne::Ld, Reg::V(0x0), Reg::V(0xF))), "ld   v0, vf");
		assert_eq!(op_to_str(Op::RegReg(Mne::Ld, Reg::V(0xF), Reg::V(0x0))), "ld   vf, v0");

		assert_eq!(op_to_str(Op::RegAddr(Mne::Ld, Reg::I, 0x000)), "ld   i, 0x000");
		assert_eq!(op_to_str(Op::RegAddr(Mne::Ld, Reg::I, 0x69E)), "ld   i, 0x69e");
		assert_eq!(op_to_str(Op::RegAddr(Mne::Ld, Reg::I, 0xFFF)), "ld   i, 0xfff");

		assert_eq!(op_to_str(Op::RegReg(Mne::Ld, Reg::V(0x0), Reg::Dt)), "ld   v0, dt");
		assert_eq!(op_to_str(Op::RegReg(Mne::Ld, Reg::V(0xF), Reg::Dt)), "ld   vf, dt");

		assert_eq!(op_to_str(Op::RegReg(Mne::Ld, Reg::V(0x0), Reg::K)), "ld   v0, k");
		assert_eq!(op_to_str(Op::RegReg(Mne::Ld, Reg::V(0xF), Reg::K)), "ld   vf, k");

		assert_eq!(op_to_str(Op::RegReg(Mne::Ld, Reg::Dt, Reg::V(0x0))), "ld   dt, v0");
		assert_eq!(op_to_str(Op::RegReg(Mne::Ld, Reg::Dt, Reg::V(0xF))), "ld   dt, vf");

		assert_eq!(op_to_str(Op::RegReg(Mne::Ld, Reg::St, Reg::V(0x0))), "ld   st, v0");
		assert_eq!(op_to_str(Op::RegReg(Mne::Ld, Reg::St, Reg::V(0xF))), "ld   st, vf");

		assert_eq!(op_to_str(Op::RegReg(Mne::Ld, Reg::F, Reg::V(0x0))), "ld   f, v0");
		assert_eq!(op_to_str(Op::RegReg(Mne::Ld, Reg::F, Reg::V(0xF))), "ld   f, vf");

		assert_eq!(op_to_str(Op::RegReg(Mne::Ld, Reg::B, Reg::V(0x0))), "ld   b, v0");
		assert_eq!(op_to_str(Op::RegReg(Mne::Ld, Reg::B, Reg::V(0xF))), "ld   b, vf");

		assert_eq!(op_to_str(Op::IndirectReg(Mne::Ld, Reg::V(0x0))), "ld   [i], v0");
		assert_eq!(op_to_str(Op::IndirectReg(Mne::Ld, Reg::V(0xF))), "ld   [i], vf");

		assert_eq!(op_to_str(Op::RegIndirect(Mne::Ld, Reg::V(0x0))), "ld   v0, [i]");
		assert_eq!(op_to_str(Op::RegIndirect(Mne::Ld, Reg::V(0xF))), "ld   vf, [i]");
	}

	#[test]
	fn disasm_or() {
		assert_eq!(op_to_str(Op::RegReg(Mne::Or, Reg::V(0x0), Reg::V(0xF))), "or   v0, vf");
		assert_eq!(op_to_str(Op::RegReg(Mne::Or, Reg::V(0xF), Reg::V(0x0))), "or   vf, v0");
	}

	#[test]
	fn disasm_ret() {
		assert_eq!(op_to_str(Op::Implied(Mne::Ret)), "ret");
	}

	#[test]
	fn disasm_rnd() {
		assert_eq!(op_to_str(Op::RegImmU8(Mne::Rnd, Reg::V(0x0), 0xFF)), "rnd  v0, 0xff");
		assert_eq!(op_to_str(Op::RegImmU8(Mne::Rnd, Reg::V(0xF), 0x00)), "rnd  vf, 0");
	}

	#[test]
	fn disasm_se() {
		assert_eq!(op_to_str(Op::RegImmU8(Mne::Se, Reg::V(0x0), 0x00)), "se   v0, 0");
		assert_eq!(op_to_str(Op::RegImmU8(Mne::Se, Reg::V(0x5), 0x67)), "se   v5, 0x67");
		assert_eq!(op_to_str(Op::RegImmU8(Mne::Se, Reg::V(0xF), 0xFF)), "se   vf, 0xff");

		assert_eq!(op_to_str(Op::RegReg(Mne::Se, Reg::V(0x0), Reg::V(0xF))), "se   v0, vf");
		assert_eq!(op_to_str(Op::RegReg(Mne::Se, Reg::V(0xF), Reg::V(0x0))), "se   vf, v0");
	}

	#[test]
	fn disasm_shl() {
		assert_eq!(op_to_str(Op::RegSorta(Mne::Shl, Reg::V(0x0), Reg::V(0xF))), "shl  v0 {, vf}");
		assert_eq!(op_to_str(Op::RegSorta(Mne::Shl, Reg::V(0xF), Reg::V(0x0))), "shl  vf {, v0}");
	}

	#[test]
	fn disasm_shr() {
		assert_eq!(op_to_str(Op::RegSorta(Mne::Shr, Reg::V(0x0), Reg::V(0xF))), "shr  v0 {, vf}");
		assert_eq!(op_to_str(Op::RegSorta(Mne::Shr, Reg::V(0xF), Reg::V(0x0))), "shr  vf {, v0}");
	}

	#[test]
	fn disasm_sknp() {
		assert_eq!(op_to_str(Op::Reg(Mne::Sknp, Reg::V(0x0))), "sknp v0");
		assert_eq!(op_to_str(Op::Reg(Mne::Sknp, Reg::V(0xF))), "sknp vf");
	}

	#[test]
	fn disasm_skp() {
		assert_eq!(op_to_str(Op::Reg(Mne::Skp, Reg::V(0x0))), "skp  v0");
		assert_eq!(op_to_str(Op::Reg(Mne::Skp, Reg::V(0xF))), "skp  vf");
	}

	#[test]
	fn disasm_sne() {
		assert_eq!(op_to_str(Op::RegImmU8(Mne::Sne, Reg::V(0x0), 0x00)), "sne  v0, 0");
		assert_eq!(op_to_str(Op::RegImmU8(Mne::Sne, Reg::V(0xA), 0x45)), "sne  va, 0x45");
		assert_eq!(op_to_str(Op::RegImmU8(Mne::Sne, Reg::V(0xF), 0xFF)), "sne  vf, 0xff");

		assert_eq!(op_to_str(Op::RegReg(Mne::Sne, Reg::V(0x0), Reg::V(0xF))), "sne  v0, vf");
		assert_eq!(op_to_str(Op::RegReg(Mne::Sne, Reg::V(0xF), Reg::V(0x0))), "sne  vf, v0");
	}

	#[test]
	fn disasm_sub() {
		assert_eq!(op_to_str(Op::RegReg(Mne::Sub, Reg::V(0x0), Reg::V(0xF))), "sub  v0, vf");
		assert_eq!(op_to_str(Op::RegReg(Mne::Sub, Reg::V(0xF), Reg::V(0x0))), "sub  vf, v0");
	}

	#[test]
	fn disasm_subn() {
		assert_eq!(op_to_str(Op::RegReg(Mne::Subn, Reg::V(0x0), Reg::V(0xF))), "subn v0, vf");
		assert_eq!(op_to_str(Op::RegReg(Mne::Subn, Reg::V(0xF), Reg::V(0x0))), "subn vf, v0");
	}

	#[test]
	fn disasm_sys() {
		assert_eq!(op_to_str(Op::Addr(Mne::Sys, 0x00)), "sys  0x000");
		assert_eq!(op_to_str(Op::Addr(Mne::Sys, 0x10)), "sys  0x010");
		assert_eq!(op_to_str(Op::Addr(Mne::Sys, 0xDF)), "sys  0x0df");
		assert_eq!(op_to_str(Op::Addr(Mne::Sys, 0xF0)), "sys  0x0f0");
		assert_eq!(op_to_str(Op::Addr(Mne::Sys, 0xFF)), "sys  0x0ff");
	}

	#[test]
	fn disasm_xor() {
		assert_eq!(op_to_str(Op::RegReg(Mne::Xor, Reg::V(0x0), Reg::V(0xF))), "xor  v0, vf");
		assert_eq!(op_to_str(Op::RegReg(Mne::Xor, Reg::V(0xF), Reg::V(0x0))), "xor  vf, v0");
	}
}


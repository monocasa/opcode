use super::{Addr, Disassembler, DisError, DisResult};

pub enum Uarch {
	Gekko,
	Xenon,
	Ppc206B,
}

#[derive(Debug, PartialEq)]
pub enum Reg {
	Gpr(u8),
	LiteralZero,
	Cr,
	CrBit(u8),
	Lr,
	Ctr,
	Xer,
	VrSave,
	Fpr(u8),
	Fpscr,
	Vr(u8),
	Vscr,
	Spr(u16),
}

#[derive(Debug, PartialEq)]
pub enum Mne {
	Addi,
	Addis,
	Isync,
	Lbz,
	Lbzu,
	Lbzux,
	Lbzx,
	Lha,
	Lhau,
	Lhaux,
	Lhax,
	Lhz,
	Lhzu,
	Lhzux,
	Lhzx,
	Lwz,
	Lwzu,
	Lwzux,
	Lwzx,
	Mtmsrd,
	Mtspr,
}

#[derive(Debug, PartialEq)]
pub enum Op {
	Implied(Mne),
	RsL(Mne, Reg, u8),
	RtDRa(Mne, Reg, i16, Reg),
	RtRaRb(Mne, Reg, Reg, Reg),
	SprRs(Mne, Reg, Reg),
}

fn mne_to_str(mne: &Mne) -> String {
	match mne {
		&Mne::Addi   => "addi",
		&Mne::Addis  => "addis",
		&Mne::Isync  => "isync",
		&Mne::Lbz    => "lbz",
		&Mne::Lbzu   => "lbzu",
		&Mne::Lbzux  => "lbzux",
		&Mne::Lbzx   => "lbzx",
		&Mne::Lha    => "lha",
		&Mne::Lhau   => "lhau",
		&Mne::Lhaux  => "lhaux",
		&Mne::Lhax   => "lhax",
		&Mne::Lhz    => "lhz",
		&Mne::Lhzu   => "lhzu",
		&Mne::Lhzux  => "lhzux",
		&Mne::Lhzx   => "lhzx",
		&Mne::Lwz    => "lwz",
		&Mne::Lwzu   => "lwzu",
		&Mne::Lwzux  => "lwzux",
		&Mne::Lwzx   => "lwzx",
		&Mne::Mtmsrd => "mtmsrd",
		&Mne::Mtspr  => "mtspr",
	}.to_string()
}

fn reg_to_str(reg: &Reg) -> String {
	match reg {
 		&Reg::Gpr(gpr)    => format!("r{}", gpr),
		&Reg::LiteralZero => format!("0"),
		&Reg::Spr(spr)    => format!("{}", spr),
		_             => format!("wat"),
	}
}

pub fn op_to_str(op: &Op) -> String {
	match op {
		&Op::Implied(ref mne)                        => format!("{}", mne_to_str(&mne)),

		&Op::RsL(ref mne, ref rs, ref l)             => format!("{:<7} {},{}",
		                                                        mne_to_str(&mne),
		                                                        reg_to_str(&rs),
		                                                        l),

		&Op::RtDRa(ref mne, ref rt, ref d, ref ra)   => format!("{:<7} {},{}({})",
		                                                        mne_to_str(&mne),
		                                                        reg_to_str(&rt), 
		                                                        d, 
		                                                        reg_to_str(&ra)),

		&Op::RtRaRb(ref mne, ref rt, ref ra, ref rb) => format!("{:<7} {},{},{}",
		                                                        mne_to_str(&mne),
		                                                        reg_to_str(&rt),
		                                                        reg_to_str(&ra),
		                                                        reg_to_str(&rb)),

		&Op::SprRs(ref mne, ref spr, ref rs) => format!("{:<7} {},{}",
		                                                mne_to_str(&mne),
		                                                reg_to_str(&spr),
		                                                reg_to_str(&rs)),
	}
}

fn opcd(instr: u32) -> u16 {
	((instr >> 26) & 0x3F) as u16
}

fn d_rt(instr: u32) -> Reg {
	Reg::Gpr(((instr >> 21) & 0x1F) as u8)
}

fn d_ra(instr: u32) -> Reg {
	Reg::Gpr(((instr >> 16) & 0x1F) as u8)
}

fn d_d(instr: u32) -> i16 {
	(instr & 0xFFFF) as i16
}

fn x_ra(instr: u32) -> Reg {
	Reg::Gpr(((instr >> 16) & 0x1F) as u8)
}

fn x_rb(instr: u32) -> Reg {
	Reg::Gpr(((instr >> 11) & 0x1F) as u8)
}

fn x_rt(instr: u32) -> Reg {
	Reg::Gpr(((instr >> 21) & 0x1F) as u8)
}

fn x_rs(instr: u32) -> Reg {
	Reg::Gpr(((instr >> 21) & 0x1F) as u8)
}

fn x_xo(instr: u32) -> u16 {
	((instr >> 1) & 0x3FF) as u16
}

fn xfx_spr(instr: u32) -> Reg {
	Reg::Spr( (((instr >> 6) & 0x3E0) | ((instr >> 16) & 0x1F)) as u16)
}

fn ra_or_literal_zero(reg: Reg) -> Reg {
	match reg {
		Reg::Gpr(0) => Reg::LiteralZero,
		_           => reg,
	}
}

fn d_rtdra(mne: Mne, instr: u32) -> Op {
	Op::RtDRa(mne, d_rt(instr), d_d(instr), d_ra(instr))
}

fn d_rtdral(mne: Mne, instr: u32) -> Op {
	Op::RtDRa(mne, d_rt(instr), d_d(instr), ra_or_literal_zero(d_ra(instr)))
}

fn x_rtrarb(mne: Mne, instr: u32) -> Op {
	Op::RtRaRb(mne, x_rt(instr), x_ra(instr), x_rb(instr))
}

fn x_rtralrb(mne: Mne, instr: u32) -> Op {
	Op::RtRaRb(mne, x_rt(instr), ra_or_literal_zero(x_ra(instr)), x_rb(instr))
}

fn xfx_sprrs(mne: Mne, instr: u32) -> Op {
	Op::SprRs(mne, xfx_spr(instr), x_rs(instr))
}

fn xl_mtmsrd(instr: u32) -> Op {
	Op::RsL(Mne::Mtmsrd, x_rs(instr), ((instr >> 16) & 1) as u8)
}

#[allow(unused)]
fn decode_op_19(instr: u32, addr: Addr, uarch: Uarch) -> Result<Op, DisError> {
	let op = match x_xo(instr) {
		150 => Op::Implied(Mne::Isync),

		_ => return Err(DisError::Unknown{num_bytes: 4}),
	};

	Ok(op)
}

#[allow(unused)]
fn decode_special(instr: u32, addr: Addr, uarch: Uarch) -> Result<Op, DisError> {
	let op = match x_xo(instr) {
		23  => x_rtralrb(Mne::Lwzx,  instr),

		55  => x_rtrarb( Mne::Lwzux, instr),

		87  => x_rtralrb(Mne::Lbzx,  instr),

		119 => x_rtrarb( Mne::Lbzux, instr),

		178 => xl_mtmsrd(instr),

		279 => x_rtralrb(Mne::Lhzx,  instr),

		311 => x_rtrarb( Mne::Lhzux, instr),

		343 => x_rtralrb(Mne::Lhax,  instr),

		375 => x_rtrarb( Mne::Lhaux, instr),

		467 => xfx_sprrs(Mne::Mtspr, instr),

		_ => return Err(DisError::Unknown{num_bytes: 4}),
	};

	Ok(op)
}

#[allow(unused)]
pub fn decode(instr: u32, addr: Addr, uarch: Uarch) -> Result<Op, DisError> {
	let op = match opcd(instr) {
		19 => return decode_op_19(instr, addr, uarch),

		31 => return decode_special(instr, addr, uarch),

		32 => d_rtdral(Mne::Lwz, instr),
		33 => d_rtdra(Mne::Lwzu, instr),
		34 => d_rtdral(Mne::Lbz, instr),
		35 => d_rtdra(Mne::Lbzu, instr),

		40 => d_rtdral(Mne::Lhz, instr),
		41 => d_rtdra(Mne::Lhzu, instr),
		42 => d_rtdral(Mne::Lha, instr),
		43 => d_rtdra(Mne::Lhau, instr),

		_ => return Err(DisError::Unknown{num_bytes: 4}),
	};

	Ok(op)
}

pub struct PpcDisasm;

impl Disassembler for PpcDisasm {
	#[allow(unused_variables)]
	fn disassemble(&self, addr: Addr, buf: &[u8]) -> DisResult  {
		if buf.len() < 4 {
			return Err(DisError::MemOverflow);
		}

		if (addr % 4) != 0 {
			return Err( DisError::Unaligned{ desired_alignment: 4, } );
		}

		let instr = ((buf[0] as u32) << 24) |
		            ((buf[1] as u32) << 16) |
		            ((buf[2] as u32) << 8 ) |
		            ((buf[3] as u32) << 0 );

		let op = try!(decode(instr, addr, Uarch::Ppc206B));

		Ok((op_to_str(&op), 4))
	}

	fn bytes_per_unit(&self) -> u16 {
		1
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	use super::super::Disassembler;

	enum TestCase {
		Normal{ instr: u32, asm: &'static str, op: Op },
	}

	static TEST_CASES: [TestCase; 48] = [
		TestCase::Normal{ instr: 0x4C00012C, asm: "isync",                  op: Op::Implied(Mne::Isync) },

		TestCase::Normal{ instr: 0x89230080, asm: "lbz     r9,128(r3)",     op: Op::RtDRa(Mne::Lbz, Reg::Gpr(9),    128, Reg::Gpr( 3)) },
		TestCase::Normal{ instr: 0x88EAB004, asm: "lbz     r7,-20476(r10)", op: Op::RtDRa(Mne::Lbz, Reg::Gpr(7), -20476, Reg::Gpr(10)) },
		TestCase::Normal{ instr: 0x88000000, asm: "lbz     r0,0(0)",        op: Op::RtDRa(Mne::Lbz, Reg::Gpr(0),      0, Reg::LiteralZero) },

		TestCase::Normal{ instr: 0x8D23FFFF, asm: "lbzu    r9,-1(r3)",      op :Op::RtDRa(Mne::Lbzu, Reg::Gpr(9),-1,Reg::Gpr(3)) },
		TestCase::Normal{ instr: 0x8C000000, asm: "lbzu    r0,0(r0)",       op :Op::RtDRa(Mne::Lbzu, Reg::Gpr(0),0,Reg::Gpr(0)) },

		TestCase::Normal{ instr: 0x7D2348EE, asm: "lbzux   r9,r3,r9",       op: Op::RtRaRb(Mne::Lbzux, Reg::Gpr(9), Reg::Gpr( 3), Reg::Gpr(9)) },
		TestCase::Normal{ instr: 0x7D0A40EE, asm: "lbzux   r8,r10,r8",      op: Op::RtRaRb(Mne::Lbzux, Reg::Gpr(8), Reg::Gpr(10), Reg::Gpr(8)) },
		TestCase::Normal{ instr: 0x7C0000EE, asm: "lbzux   r0,r0,r0",       op: Op::RtRaRb(Mne::Lbzux, Reg::Gpr(0), Reg::Gpr(0), Reg::Gpr(0)) },

		TestCase::Normal{ instr: 0x7C7F50AE, asm: "lbzx    r3,r31,r10",     op: Op::RtRaRb(Mne::Lbzx, Reg::Gpr( 3), Reg::Gpr(31), Reg::Gpr(10)) },
		TestCase::Normal{ instr: 0x7D4340AE, asm: "lbzx    r10,r3,r8",      op: Op::RtRaRb(Mne::Lbzx, Reg::Gpr(10), Reg::Gpr( 3), Reg::Gpr( 8)) },
		TestCase::Normal{ instr: 0x7C0000AE, asm: "lbzx    r0,0,r0",        op: Op::RtRaRb(Mne::Lbzx, Reg::Gpr(0), Reg::LiteralZero, Reg::Gpr(0)) },

		TestCase::Normal{ instr: 0xA8050000, asm: "lha     r0,0(r5)",       op: Op::RtDRa(Mne::Lha, Reg::Gpr(0), 0, Reg::Gpr(5)) },
		TestCase::Normal{ instr: 0xA8E70002, asm: "lha     r7,2(r7)",       op: Op::RtDRa(Mne::Lha, Reg::Gpr(7), 2, Reg::Gpr(7)) },
		TestCase::Normal{ instr: 0xA8000000, asm: "lha     r0,0(0)",        op: Op::RtDRa(Mne::Lha, Reg::Gpr(0), 0, Reg::LiteralZero) },

		TestCase::Normal{ instr: 0xACE80002, asm: "lhau    r7,2(r8)",       op: Op::RtDRa(Mne::Lhau, Reg::Gpr(7), 2, Reg::Gpr(8)) },
		TestCase::Normal{ instr: 0xACC40002, asm: "lhau    r6,2(r4)",       op: Op::RtDRa(Mne::Lhau, Reg::Gpr(6), 2, Reg::Gpr(4)) },
		TestCase::Normal{ instr: 0xAC000000, asm: "lhau    r0,0(r0)",       op: Op::RtDRa(Mne::Lhau, Reg::Gpr(0), 0, Reg::Gpr(0)) },

		TestCase::Normal{ instr: 0x7C7F52EE, asm: "lhaux   r3,r31,r10",     op: Op::RtRaRb(Mne::Lhaux, Reg::Gpr( 3), Reg::Gpr(31), Reg::Gpr(10)) },
		TestCase::Normal{ instr: 0x7D4342EE, asm: "lhaux   r10,r3,r8",      op: Op::RtRaRb(Mne::Lhaux, Reg::Gpr(10), Reg::Gpr( 3), Reg::Gpr( 8)) },
		TestCase::Normal{ instr: 0x7C0002EE, asm: "lhaux   r0,r0,r0",       op: Op::RtRaRb(Mne::Lhaux, Reg::Gpr(0), Reg::Gpr(0), Reg::Gpr(0)) },

		TestCase::Normal{ instr: 0x7CFB3AAE, asm: "lhax    r7,r27,r7",      op: Op::RtRaRb(Mne::Lhax, Reg::Gpr( 7), Reg::Gpr(27), Reg::Gpr(7)) },
		TestCase::Normal{ instr: 0x7DDE4AAE, asm: "lhax    r14,r30,r9",     op: Op::RtRaRb(Mne::Lhax, Reg::Gpr(14), Reg::Gpr(30), Reg::Gpr(9)) },
		TestCase::Normal{ instr: 0x7C0002AE, asm: "lhax    r0,0,r0",        op: Op::RtRaRb(Mne::Lhax, Reg::Gpr(0), Reg::LiteralZero, Reg::Gpr(0)) },

		TestCase::Normal{ instr: 0xA09E0008, asm: "lhz     r4,8(r30)",      op: Op::RtDRa(Mne::Lhz, Reg::Gpr(4),      8, Reg::Gpr(30)) },
		TestCase::Normal{ instr: 0xA0E9B328, asm: "lhz     r7,-19672(r9)",  op: Op::RtDRa(Mne::Lhz, Reg::Gpr(7), -19672, Reg::Gpr( 9)) },
		TestCase::Normal{ instr: 0xA0000000, asm: "lhz     r0,0(0)",        op: Op::RtDRa(Mne::Lhz, Reg::Gpr(0), 0, Reg::LiteralZero) },

		TestCase::Normal{ instr: 0xA4E80002, asm: "lhzu    r7,2(r8)",       op: Op::RtDRa(Mne::Lhzu, Reg::Gpr(7), 2, Reg::Gpr(8)) },
		TestCase::Normal{ instr: 0xA4000000, asm: "lhzu    r0,0(r0)",       op: Op::RtDRa(Mne::Lhzu, Reg::Gpr(0), 0, Reg::Gpr(0)) },

		TestCase::Normal{ instr: 0x7D44A26E, asm: "lhzux   r10,r4,r20",     op: Op::RtRaRb(Mne::Lhzux, Reg::Gpr(10), Reg::Gpr(4), Reg::Gpr(20)) },
		TestCase::Normal{ instr: 0x7C00026E, asm: "lhzux   r0,r0,r0",       op: Op::RtRaRb(Mne::Lhzux, Reg::Gpr(0), Reg::Gpr(0), Reg::Gpr(0)) },

		TestCase::Normal{ instr: 0x7D674A2E, asm: "lhzx    r11,r7,r9",      op: Op::RtRaRb(Mne::Lhzx, Reg::Gpr(11), Reg::Gpr(7), Reg::Gpr(9)) },
		TestCase::Normal{ instr: 0x7CC64A2E, asm: "lhzx    r6,r6,r9",       op: Op::RtRaRb(Mne::Lhzx, Reg::Gpr( 6), Reg::Gpr(6), Reg::Gpr(9)) },
		TestCase::Normal{ instr: 0x7C00022E, asm: "lhzx    r0,0,r0",        op: Op::RtRaRb(Mne::Lhzx, Reg::Gpr(0), Reg::LiteralZero, Reg::Gpr(0)) },

		TestCase::Normal{ instr: 0x812985B0, asm: "lwz     r9,-31312(r9)",  op: Op::RtDRa(Mne::Lwz, Reg::Gpr(9), -31312, Reg::Gpr(9)) },
		TestCase::Normal{ instr: 0x80890008, asm: "lwz     r4,8(r9)",       op: Op::RtDRa(Mne::Lwz, Reg::Gpr(4),      8, Reg::Gpr(9)) },
		TestCase::Normal{ instr: 0x80000000, asm: "lwz     r0,0(0)",        op: Op::RtDRa(Mne::Lwz, Reg::Gpr(0), 0, Reg::LiteralZero) },

		TestCase::Normal{ instr: 0x847F0004, asm: "lwzu    r3,4(r31)",      op: Op::RtDRa(Mne::Lwzu, Reg::Gpr(3), 4, Reg::Gpr(31)) },
		TestCase::Normal{ instr: 0x85060004, asm: "lwzu    r8,4(r6)",       op: Op::RtDRa(Mne::Lwzu, Reg::Gpr(8), 4, Reg::Gpr( 6)) },
		TestCase::Normal{ instr: 0x84000000, asm: "lwzu    r0,0(r0)",       op: Op::RtDRa(Mne::Lwzu, Reg::Gpr(0), 0, Reg::Gpr(0)) },

		TestCase::Normal{ instr: 0x7D3F486E, asm: "lwzux   r9,r31,r9",      op: Op::RtRaRb(Mne::Lwzux, Reg::Gpr(9), Reg::Gpr(31), Reg::Gpr(9)) },
		TestCase::Normal{ instr: 0x7C00006E, asm: "lwzux   r0,r0,r0",       op: Op::RtRaRb(Mne::Lwzux, Reg::Gpr(0), Reg::Gpr(0), Reg::Gpr(0)) },

		TestCase::Normal{ instr: 0x7F8A482E, asm: "lwzx    r28,r10,r9",     op: Op::RtRaRb(Mne::Lwzx, Reg::Gpr(28), Reg::Gpr(10), Reg::Gpr( 9)) },
		TestCase::Normal{ instr: 0x7D09502E, asm: "lwzx    r8,r9,r10",      op: Op::RtRaRb(Mne::Lwzx, Reg::Gpr( 8), Reg::Gpr( 9), Reg::Gpr(10)) },
		TestCase::Normal{ instr: 0x7C00002E, asm: "lwzx    r0,0,r0",        op: Op::RtRaRb(Mne::Lwzx, Reg::Gpr(0), Reg::LiteralZero, Reg::Gpr(0)) },

		TestCase::Normal{ instr: 0x7DA10164, asm: "mtmsrd  r13,1",          op: Op::RsL(Mne::Mtmsrd, Reg::Gpr(13), 1) },

		TestCase::Normal{ instr: 0x7C7E4BA6, asm: "mtspr   318,r3",         op: Op::SprRs(Mne::Mtspr, Reg::Spr(318), Reg::Gpr(3)) },
		TestCase::Normal{ instr: 0x7D51FBA6, asm: "mtspr   1009,r10",       op: Op::SprRs(Mne::Mtspr, Reg::Spr(1009), Reg::Gpr(10)) },
	];

	#[test]
	fn decode_instrs() {
		for test_case in TEST_CASES.iter() {
			match test_case {
				&TestCase::Normal{instr, ref op, ..} => {
					assert_eq!(&decode(instr, 0, Uarch::Ppc206B).unwrap(), op)
				},
			}
		}
	}

	#[test]
	fn disasm_instrs() {
		let mut buffer: [u8;4] = [0; 4];
		let disasm: Box<Disassembler> = Box::new(PpcDisasm);

		for test_case in TEST_CASES.iter() {
			match test_case {
				&TestCase::Normal{instr, asm, ..} => {
					buffer[0] = (instr >> 24) as u8;
					buffer[1] = (instr >> 16) as u8;
					buffer[2] = (instr >> 8)  as u8;
					buffer[3] = (instr >> 0)  as u8;

					assert_eq!(disasm.disassemble(0, &buffer),
					           Ok((asm.to_string(), 4)));
				},
			}
		}
	}
}


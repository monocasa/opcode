use super::{Addr, AddrTarget, Disassembler, DisError, DisResult};

pub enum Uarch {
	LsiR2000,       // Canonical MIPS-I processor
	HarvardMips161, // R2000 w/ LL, SC, and WAIT instructions
}

#[allow(dead_code)]
pub struct UarchInfo {
	has_wait_instr: bool,
	has_ll_sc_instr: bool,
}

const LSI_R2000_INFO: &'static UarchInfo = &UarchInfo {
	has_wait_instr: false,
	has_ll_sc_instr: false,
};

const HARVARD_MIPS161_INFO: &'static UarchInfo = &UarchInfo {
	has_wait_instr: false,
	has_ll_sc_instr: false,
};

pub fn uarch_info_for_uarch(uarch: Uarch) -> &'static UarchInfo {
	match uarch {
		Uarch::LsiR2000 => LSI_R2000_INFO,
		Uarch::HarvardMips161 => HARVARD_MIPS161_INFO,
	}
}

pub const ZERO: u8 = 0;
pub const AT:   u8 = 1;
pub const V0:   u8 = 2;
pub const V1:   u8 = 3;
pub const A0:   u8 = 4;
pub const A1:   u8 = 5;
pub const A2:   u8 = 6;
pub const A3:   u8 = 7;
pub const T0:   u8 = 8;
pub const T1:   u8 = 9;
pub const T2:   u8 = 10;
pub const T3:   u8 = 11;
pub const T4:   u8 = 12;
pub const T5:   u8 = 13;
pub const T6:   u8 = 14;
pub const T7:   u8 = 15;
pub const S0:   u8 = 16;
pub const S1:   u8 = 17;
pub const S2:   u8 = 18;
pub const S3:   u8 = 19;
pub const S4:   u8 = 20;
pub const S5:   u8 = 21;
pub const S6:   u8 = 22;
pub const S7:   u8 = 23;
pub const T8:   u8 = 24;
pub const T9:   u8 = 25;
pub const K0:   u8 = 26;
pub const K1:   u8 = 27;
pub const GP:   u8 = 28;
pub const SP:   u8 = 29;
pub const S8:   u8 = 30;
pub const RA:   u8 = 31;

#[derive(Debug, PartialEq)]
pub enum Reg {
	Gpr(u8),
	Cpr(u8),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Cop {
	C0,
	C1,
	C2,
	C3,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Mne {
	Add,
	Addi,
	Addiu,
	Addu,
	And,
	Andi,
	Bczt,
	Bczf,
	Beq,
	Bgez,
	Bgezal,
	Bgtz,
	Blez,
	Bltz,
	Bltzal,
	Bne,
	Break,
	Cfcz,
	Copz,
	Ctcz,
	Div,
	Divu,
	J,
	Jal,
	Jalr,
	Jr,
	Lb,
	Lbu,
	Lh,
	Lhu,
	Ll,
	Lui,
	Lw,
	Lwc(Cop),
	Lwl,
	Lwr,
	Mfc(Cop),
	Mfhi,
	Mflo,
	Mtc(Cop),
	Mthi,
	Mtlo,
	Mult,
	Multu,
	Nor,
	Or,
	Ori,
	Sb,
	Sc,
	Sh,
	Sll,
	Sllv,
	Slt,
	Slti,
	Sltiu,
	Sltu,
	Sra,
	Srl,
	Srlv,
	Srav,
	Sub,
	Subu,
	Sw,
	Swc(Cop),
	Swl,
	Syscall,
	Swr,
	Xor,
	Xori,

	//Base Pseudo-ops
	Li,
	Move,
	Nop,

	//COP0 Pseudo-ops
	Rfe,
	Tlbp,
	Tlbr,
	Tlbwi,
	Tlbwr,
	Wait,
}

#[derive(Debug, PartialEq)]
pub enum Op {
	Implied(Mne),
	RdRs(Mne, Reg, Reg),
	RdRsRt(Mne, Reg, Reg, Reg),
	RdRtSa(Mne, Reg, Reg, u8),
	Rs(Mne, Reg),
	RsRtTarget(Mne, Reg, Reg, AddrTarget),
	RtI16(Mne, Reg, i16),
	RtOffsetBase(Mne, Reg, i16, Reg),
	RtRd(Mne, Reg, Reg),
	RtRs(Mne, Reg, Reg),
	RtRsI16(Mne, Reg, Reg, i16),
	RtU16(Mne, Reg, u16),
	Target(Mne, AddrTarget),
}

#[allow(dead_code)]
pub struct DecodeOptions {
	decode_pseudo_ops: bool,
}

fn opcode(instr: u32) -> u8 {
	((instr >> 26) & 0x3F) as u8
}

fn cop_function(instr: u32) -> u8 {
	((instr >> 21) & 0x1F) as u8
}

fn special_function(instr: u32) -> u8 {
	(instr & 0x3F) as u8
}

fn rs(instr: u32) -> Reg {
	Reg::Gpr(((instr >> 21) & 0x1F) as u8)
}

fn rt(instr: u32) -> Reg {
	Reg::Gpr(((instr >> 16) & 0x1F) as u8)
}

fn crd(instr: u32) -> Reg {
	Reg::Cpr(((instr >> 11) & 0x1F) as u8)
}

fn rd(instr: u32) -> Reg {
	Reg::Gpr(((instr >> 11) & 0x1F) as u8)
}

fn sa(instr: u32) -> u8 {
	((instr >> 6) & 0x1F) as u8
}

fn immi16(instr: u32) -> i16 {
	(instr & 0xFFFF) as i16
}

fn immu16(instr: u32) -> u16 {
	instr as u16
}

fn cond_branch_offset(instr: u32) -> AddrTarget {
	AddrTarget::Relative((immi16(instr) as i64) << 2)
}

fn jump_target(instr: u32, addr: Addr) -> AddrTarget {
	AddrTarget::Absolute( (((instr & 0x03FF_FFFF) << 2) as u64) | (addr & 0xFFFF_FFFF_F000_0000_u64) )
}

#[allow(unused_variables)]
fn decode_special(instr: u32, uarch_info: &UarchInfo, decode_options: &DecodeOptions) -> Result<Op, DisError> {
	let op = match special_function(instr) {
		0b000000 => Op::RdRtSa(Mne::Sll, rd(instr), rt(instr), sa(instr)),

		0b001000 => Op::Rs(Mne::Jr, rs(instr)),
		0b001001 => Op::RdRs(Mne::Jalr, rd(instr), rs(instr)),

		0b100000 => Op::RdRsRt(Mne::Add,  rd(instr), rs(instr), rt(instr)),
		0b100001 => Op::RdRsRt(Mne::Addu, rd(instr), rs(instr), rt(instr)),

		_ => return Err(DisError::Unknown{num_bytes: 4}),
	};

	Ok(op)
}

fn decode_cop(cop: Cop, instr: u32) -> Result<Op, DisError> {
	let op = match cop_function(instr) {
		0b00100 => Op::RtRd(Mne::Mtc(cop), rt(instr), crd(instr)),

		_ => return Err(DisError::Unknown{num_bytes: 4}),
	};

	Ok(op)
}

fn convert_to_pseudo_op(op: Op) -> Op {
	match op {
		Op::RdRtSa(Mne::Sll, Reg::Gpr(0), Reg::Gpr(0), 0) => Op::Implied(Mne::Nop),

		Op::RdRsRt(Mne::Addu, rd, rs, Reg::Gpr(0)) => Op::RtRs(Mne::Move, rd, rs),

		Op::RtRsI16(Mne::Addiu, rt, Reg::Gpr(0), imm) => Op::RtI16(Mne::Li, rt, imm),

		_ => op,
	}
}

#[allow(unused_variables)]
pub fn decode(instr: u32, addr: Addr, uarch_info: &UarchInfo, decode_options: &DecodeOptions) -> Result<Op, DisError> {
	let op = match opcode(instr) {
		0b000000 => try!(decode_special(instr, uarch_info, decode_options)),

		0b000010 => Op::Target(Mne::J, jump_target(instr, addr)),

		0b000100 => Op::RsRtTarget(Mne::Beq, rs(instr), rt(instr), cond_branch_offset(instr)),
		0b000101 => Op::RsRtTarget(Mne::Bne, rs(instr), rt(instr), cond_branch_offset(instr)),

		0b001000 => Op::RtRsI16(Mne::Addi,  rt(instr), rs(instr), immi16(instr)),
		0b001001 => Op::RtRsI16(Mne::Addiu, rt(instr), rs(instr), immi16(instr)),
		0b001010 => Op::RtRsI16(Mne::Slti,  rt(instr), rs(instr), immi16(instr)),

		0b001111 => match rs(instr) {
			Reg::Gpr(0) => Op::RtU16(Mne::Lui, rt(instr), immu16(instr)),
			_           => return Err(DisError::Unknown{num_bytes: 4}),
		},
		0b010000 => try!(decode_cop(Cop::C0, instr)),

		0b100011 => Op::RtOffsetBase(Mne::Lw, rt(instr), immi16(instr), rs(instr)),

		0b101011 => Op::RtOffsetBase(Mne::Sw, rt(instr), immi16(instr), rs(instr)),

		_ => return Err(DisError::Unknown{num_bytes: 4}),
	};

	if decode_options.decode_pseudo_ops {
		Ok(convert_to_pseudo_op(op))
	}
	else {
		Ok(op)
	}
}

fn cop_to_num(cop: &Cop) -> u8 {
	match cop {
		&Cop::C0 => 0,
		&Cop::C1 => 1,
		&Cop::C2 => 2,
		&Cop::C3 => 3,
	}
}

fn reg_to_str(reg: &Reg) -> String {
	match reg {
		&Reg::Gpr( 0) => "zero",
		&Reg::Gpr( 1) => "at",
		&Reg::Gpr( 2) => "v0",
		&Reg::Gpr( 3) => "v1",
		&Reg::Gpr( 4) => "a0",
		&Reg::Gpr( 5) => "a1",
		&Reg::Gpr( 6) => "a2",
		&Reg::Gpr( 7) => "a3",
		&Reg::Gpr( 8) => "t0",
		&Reg::Gpr( 9) => "t1",
		&Reg::Gpr(10) => "t2",
		&Reg::Gpr(11) => "t3",
		&Reg::Gpr(12) => "t4",
		&Reg::Gpr(13) => "t5",
		&Reg::Gpr(14) => "t6",
		&Reg::Gpr(15) => "t7",
		&Reg::Gpr(16) => "s0",
		&Reg::Gpr(17) => "s1",
		&Reg::Gpr(18) => "s2",
		&Reg::Gpr(19) => "s3",
		&Reg::Gpr(20) => "s4",
		&Reg::Gpr(21) => "s5",
		&Reg::Gpr(22) => "s6",
		&Reg::Gpr(23) => "s7",
		&Reg::Gpr(24) => "t8",
		&Reg::Gpr(25) => "t9",
		&Reg::Gpr(26) => "k0",
		&Reg::Gpr(27) => "k1",
		&Reg::Gpr(28) => "gp",
		&Reg::Gpr(29) => "sp",
		&Reg::Gpr(30) => "s8",
		&Reg::Gpr(31) => "ra",

		&Reg::Cpr(cpr) => return format!("${}", cpr),

		&Reg::Gpr(gpr) => return format!("<error:gpr{}", gpr),
	}.to_string()
}

fn mne_to_str(mne: &Mne) -> String {
	match mne {
		&Mne::Add   => "add",
		&Mne::Addi  => "addi",
		&Mne::Addiu => "addiu",
		&Mne::Addu  => "addu",
		&Mne::Beq   => "beq",
		&Mne::Bne   => "bne",
		&Mne::J     => "j",
		&Mne::Jalr  => "jalr",
		&Mne::Jr    => "jr",
		&Mne::Lui   => "lui",
		&Mne::Lw    => "lw",
		&Mne::Sll   => "sll",
		&Mne::Slti  => "slti",
		&Mne::Sw    => "sw",

		&Mne::Mtc(ref cop) => return format!("mtc{}", cop_to_num(cop)),

		&Mne::Li    => "li",
		&Mne::Move  => "move",
		&Mne::Nop   => "nop",
		_ => "UNKNOWN_MNE",
	}.to_string()
}

fn target_to_str(addr: Addr, target: &AddrTarget) -> String {
	match target {
		&AddrTarget::Absolute(abs)   => format!("{:#x}", abs),
		&AddrTarget::Relative(rel)   => format!("{:#x}", rel + (addr as i64) + 4),
		&AddrTarget::Symbol(ref sym) => format!("{}", sym),
	}
}

fn op_to_str(addr: Addr, op: &Op) -> String {
	let (mne, args_str) = match op {
		&Op::Implied(ref mne) => (mne.clone(), None),

		&Op::RdRs(Mne::Jalr, Reg::Gpr(RA), ref rs) => {
			(Mne::Jalr, Some(format!("{}", reg_to_str(rs))))
		},

		&Op::RdRs(ref mne, ref rd, ref rs) => {
			(mne.clone(), Some(format!("{},{}", reg_to_str(rd), reg_to_str(rs))))
		},

		&Op::RdRsRt(ref mne, ref rd, ref rs, ref rt) => {
			(mne.clone(), Some(format!("{},{},{}", reg_to_str(rd), reg_to_str(rs), reg_to_str(rt))))
		},

		&Op::RdRtSa(ref mne, ref rd, ref rt, sa) => {
			(mne.clone(), Some(format!("{},{},{:#x}", reg_to_str(rd), reg_to_str(rt), sa)))
		},

		&Op::Rs(ref mne, ref rs) => {
			(mne.clone(), Some(format!("{}", reg_to_str(rs))))
		},

		&Op::RsRtTarget(ref mne, ref rs, ref rt, ref target) => {
			(mne.clone(), Some(format!("{},{},{}", reg_to_str(rs), reg_to_str(rt), target_to_str(addr, target))))
		},

		&Op::RtI16(ref mne, ref rt, imm) => {
			(mne.clone(), Some(format!("{},{}", reg_to_str(rt), imm)))
		},

		&Op::RtOffsetBase(ref mne, ref rt, offset, ref base) => {
			(mne.clone(), Some(format!("{},{}({})", reg_to_str(rt), offset, reg_to_str(base))))
		},

		&Op::RtRd(ref mne, ref rt, ref rd) => {
			(mne.clone(), Some(format!("{},{}", reg_to_str(rt), reg_to_str(rd))))
		},

		&Op::RtRsI16(ref mne, ref rt, ref rs, imm) => {
			(mne.clone(), Some(format!("{},{},{}", reg_to_str(rt), reg_to_str(rs), imm)))
		},

		&Op::RtRs(ref mne, ref rt, ref rs) => {
			(mne.clone(), Some(format!("{},{}", reg_to_str(rt), reg_to_str(rs))))
		},

		&Op::RtU16(ref mne, ref rt, imm) => {
			(mne.clone(), Some(format!("{},{:#x}", reg_to_str(rt), imm)))
		},

		&Op::Target(ref mne, ref target) => {
			(mne.clone(), Some(target_to_str(addr, target)))
		},
	};

	match args_str {
		Some(args) => format!("{:7} {}", mne_to_str(&mne), args),
		None       => format!("{}", mne_to_str(&mne)),
	}
}

#[allow(unused_variables)]
pub fn disasm(addr: Addr, buf: &[u8], uarch_info: &UarchInfo, decode_options: &DecodeOptions) -> DisResult {
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

	let op = try!(decode(instr, addr, uarch_info, decode_options));

	Ok((op_to_str(addr, &op), 4))
}

pub struct MipsDisasm;

impl Disassembler for MipsDisasm {
	fn disassemble(&self, addr: Addr, buf: &[u8]) -> DisResult  {
		let uarch_info = uarch_info_for_uarch(Uarch::HarvardMips161);

		let no_pseudo_ops = DecodeOptions { decode_pseudo_ops: true };

		disasm(addr, buf, uarch_info, &no_pseudo_ops)
	}

	fn bytes_per_unit(&self) -> u16 {
		4
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	use super::super::*;

	#[allow(dead_code)]
	enum TestCase {
		Normal{ instr: u32, asm: &'static str, op: Op },
		Branch{ addr: Addr, instr: u32, asm: &'static str, op: Op },
	}

	static BASE_TEST_CASES: [TestCase; 32] = [
		TestCase::Normal{ instr: 0x02024020, asm: "add     t0,s0,v0",      op: Op::RdRsRt(Mne::Add, Reg::Gpr(T0), Reg::Gpr(S0), Reg::Gpr(V0)) },

		TestCase::Normal{ instr: 0x03A0F021, asm: "addu    s8,sp,zero",    op: Op::RdRsRt(Mne::Addu, Reg::Gpr(S8), Reg::Gpr(SP), Reg::Gpr(ZERO)) },

		TestCase::Normal{ instr: 0x20101F81, asm: "addi    s0,zero,8065",  op: Op::RtRsI16(Mne::Addi, Reg::Gpr(S0), Reg::Gpr(ZERO), 8065) },
		TestCase::Normal{ instr: 0x2231FFFF, asm: "addi    s1,s1,-1",      op: Op::RtRsI16(Mne::Addi, Reg::Gpr(S1), Reg::Gpr(S1), -1) },

		TestCase::Normal{ instr: 0x27BDFFE8, asm: "addiu   sp,sp,-24",     op: Op::RtRsI16(Mne::Addiu, Reg::Gpr(SP), Reg::Gpr(SP), -24) },
		TestCase::Normal{ instr: 0x24020020, asm: "addiu   v0,zero,32",    op: Op::RtRsI16(Mne::Addiu, Reg::Gpr(V0), Reg::Gpr(ZERO), 32) },

		TestCase::Normal{ instr: 0x0060F809, asm: "jalr    v1",            op: Op::RdRs(Mne::Jalr, Reg::Gpr(RA), Reg::Gpr(V1)) },
		TestCase::Normal{ instr: 0x00C0F809, asm: "jalr    a2",            op: Op::RdRs(Mne::Jalr, Reg::Gpr(RA), Reg::Gpr(A2)) },
		TestCase::Normal{ instr: 0x00C04809, asm: "jalr    t1,a2",         op: Op::RdRs(Mne::Jalr, Reg::Gpr(T1), Reg::Gpr(A2)) },

		TestCase::Normal{ instr: 0x00400008, asm: "jr      v0",            op: Op::Rs(Mne::Jr, Reg::Gpr(V0)) },
		TestCase::Normal{ instr: 0x03E00008, asm: "jr      ra",            op: Op::Rs(Mne::Jr, Reg::Gpr(RA)) },

		TestCase::Normal{ instr: 0x3C0AA470, asm: "lui     t2,0xa470",     op: Op::RtU16(Mne::Lui, Reg::Gpr(T2), 0xA470) },
		TestCase::Normal{ instr: 0x3C1F0010, asm: "lui     ra,0x10",       op: Op::RtU16(Mne::Lui, Reg::Gpr(RA), 0x10) },

		TestCase::Normal{ instr: 0x8C43BB90, asm: "lw      v1,-17520(v0)", op: Op::RtOffsetBase(Mne::Lw, Reg::Gpr(V1), -17520, Reg::Gpr(V0)) },
		TestCase::Normal{ instr: 0x8C430000, asm: "lw      v1,0(v0)",      op: Op::RtOffsetBase(Mne::Lw, Reg::Gpr(V1),      0, Reg::Gpr(V0)) },
		TestCase::Normal{ instr: 0x8FC20018, asm: "lw      v0,24(s8)",     op: Op::RtOffsetBase(Mne::Lw, Reg::Gpr(V0),     24, Reg::Gpr(S8)) },
		TestCase::Normal{ instr: 0x8FBF0014, asm: "lw      ra,20(sp)",     op: Op::RtOffsetBase(Mne::Lw, Reg::Gpr(RA),     20, Reg::Gpr(SP)) },

		TestCase::Normal{ instr: 0x40806800, asm: "mtc0    zero,$13",      op: Op::RtRd(Mne::Mtc(Cop::C0), Reg::Gpr(0), Reg::Cpr(13)) },
		TestCase::Normal{ instr: 0x40804800, asm: "mtc0    zero,$9",       op: Op::RtRd(Mne::Mtc(Cop::C0), Reg::Gpr(0), Reg::Cpr(9)) },
		TestCase::Normal{ instr: 0x40805800, asm: "mtc0    zero,$11",      op: Op::RtRd(Mne::Mtc(Cop::C0), Reg::Gpr(0), Reg::Cpr(11)) },

		TestCase::Normal{ instr: 0x00021400, asm: "sll     v0,v0,0x10",    op: Op::RdRtSa(Mne::Sll, Reg::Gpr(V0), Reg::Gpr(V0), 0x10) },
		TestCase::Normal{ instr: 0x00000000, asm: "sll     zero,zero,0x0", op: Op::RdRtSa(Mne::Sll, Reg::Gpr(ZERO), Reg::Gpr(ZERO), 0) },

		TestCase::Normal{ instr: 0x28620031, asm: "slti    v0,v1,49",      op: Op::RtRsI16(Mne::Slti, Reg::Gpr(V0), Reg::Gpr(V1), 49) },

		TestCase::Normal{ instr: 0xAFBF0014, asm: "sw      ra,20(sp)",     op: Op::RtOffsetBase(Mne::Sw, Reg::Gpr(RA), 20, Reg::Gpr(SP)) },
		TestCase::Normal{ instr: 0xAFBE0010, asm: "sw      s8,16(sp)",     op: Op::RtOffsetBase(Mne::Sw, Reg::Gpr(S8), 16, Reg::Gpr(SP)) },
		TestCase::Normal{ instr: 0xAFC00030, asm: "sw      zero,48(s8)",   op: Op::RtOffsetBase(Mne::Sw, Reg::Gpr( 0), 48, Reg::Gpr(S8)) },

		TestCase::Branch{ addr: 0x80000368, instr: 0x10620033, asm: "beq     v1,v0,0x80000438", op: Op::RsRtTarget(Mne::Beq, Reg::Gpr(V1), Reg::Gpr(V0), AddrTarget::Relative(204)) },
		TestCase::Branch{ addr: 0x80001424, instr: 0x1062FFF7, asm: "beq     v1,v0,0x80001404", op: Op::RsRtTarget(Mne::Beq, Reg::Gpr(V1), Reg::Gpr(V0), AddrTarget::Relative(-36)) },
		TestCase::Branch{ addr: 0x80001BFC, instr: 0x1082FFE8, asm: "beq     a0,v0,0x80001ba0", op: Op::RsRtTarget(Mne::Beq, Reg::Gpr(A0), Reg::Gpr(V0), AddrTarget::Relative(-96)) },

		TestCase::Branch{ addr: 0x8001CB34, instr: 0x1443000C, asm: "bne     v0,v1,0x8001cb68", op: Op::RsRtTarget(Mne::Bne, Reg::Gpr(V0), Reg::Gpr(V1), AddrTarget::Relative( 48)) },
		TestCase::Branch{ addr: 0x80029890, instr: 0x1501FFF2, asm: "bne     t0,at,0x8002985c", op: Op::RsRtTarget(Mne::Bne, Reg::Gpr(T0), Reg::Gpr(AT), AddrTarget::Relative(-56)) },

		TestCase::Branch{ addr: 0x80000914, instr: 0x0800024a, asm: "j       0x80000928",       op: Op::Target(Mne::J, AddrTarget::Absolute(0x80000928)) },
	];

	#[test]
	fn decode_base() {
		let no_pseudo_ops = DecodeOptions { decode_pseudo_ops: false };

		let uarch_info = uarch_info_for_uarch(Uarch::HarvardMips161);

		for test_case in BASE_TEST_CASES.iter() {
			match test_case {
				&TestCase::Normal{instr, ref op, ..} => {
					assert_eq!(&decode(instr, 0, uarch_info, &no_pseudo_ops).unwrap(), op)
				},

				&TestCase::Branch{addr, instr, ref op, ..} => {
					assert_eq!(&decode(instr, addr, uarch_info, &no_pseudo_ops).unwrap(), op)
				},
			}
		}
	}

	#[test]
	fn disasm_base() {
		let no_pseudo_ops = DecodeOptions { decode_pseudo_ops: false };

		let uarch_info = uarch_info_for_uarch(Uarch::HarvardMips161);

		let mut buffer: [u8;4] = [0; 4];

		for test_case in BASE_TEST_CASES.iter() {
			match test_case {
				&TestCase::Normal{instr, ref asm, ..} => {
					buffer[0] = (instr >> 24) as u8;
					buffer[1] = (instr >> 16) as u8;
					buffer[2] = (instr >> 8)  as u8;
					buffer[3] = (instr >> 0)  as u8;

					assert_eq!(disasm(0, &buffer, uarch_info, &no_pseudo_ops),
					           Ok((asm.to_string(), 4)));
				},

				&TestCase::Branch{addr, instr, ref asm, ..} => {
					buffer[0] = (instr >> 24) as u8;
					buffer[1] = (instr >> 16) as u8;
					buffer[2] = (instr >> 8)  as u8;
					buffer[3] = (instr >> 0)  as u8;

					assert_eq!(disasm(addr, &buffer, uarch_info, &no_pseudo_ops),
					           Ok((asm.to_string(), 4)));
				},
			}
		}
	}

	static PSEUDO_OP_TEST_CASES: [TestCase; 3] = [
		TestCase::Normal{ instr: 0x24020020, asm: "li      v0,32", op: Op::RtI16(Mne::Li, Reg::Gpr(V0), 32) },

		TestCase::Normal{ instr: 0x00000000, asm: "nop",           op: Op::Implied(Mne::Nop) },

		TestCase::Normal{ instr: 0x03A0F021, asm: "move    s8,sp", op: Op::RtRs(Mne::Move, Reg::Gpr(S8), Reg::Gpr(SP)) },
	];

	#[test]
	fn decode_pseudo_ops() {
		let pseudo_ops = DecodeOptions { decode_pseudo_ops: true };

		let uarch_info = uarch_info_for_uarch(Uarch::HarvardMips161);

		for test_case in PSEUDO_OP_TEST_CASES.iter() {
			match test_case {
				&TestCase::Normal{instr, ref op, ..} => {
					assert_eq!(&decode(instr, 0, uarch_info, &pseudo_ops).unwrap(), op)
				},

				_ => {},
			}
		}
	}

	#[test]
	fn disasm_pseudo_ops() {
		let pseudo_ops = DecodeOptions { decode_pseudo_ops: true };

		let uarch_info = uarch_info_for_uarch(Uarch::HarvardMips161);

		let mut buffer: [u8;4] = [0; 4];

		for test_case in PSEUDO_OP_TEST_CASES.iter() {
			match test_case {
				&TestCase::Normal{instr, ref asm, ..} => {
					buffer[0] = (instr >> 24) as u8;
					buffer[1] = (instr >> 16) as u8;
					buffer[2] = (instr >> 8)  as u8;
					buffer[3] = (instr >> 0)  as u8;

					assert_eq!(disasm(0, &buffer, uarch_info, &pseudo_ops),
					           Ok((asm.to_string(), 4)));
				},

				_ => {},
			}
		}
	}
}


use super::{Addr, Disassembler, DisError, DisResult};

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
	Lwcz,
	Lwl,
	Lwr,
	Mfcz,
	Mfhi,
	Mflo,
	Mtcz,
	Mthi,
	Mtlo,
	Mult,
	Multu,
	Nor,
	Or,
	Ori,
	Rfe,
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
	Swcz,
	Swl,
	Syscall,
	Tlbr,
	Tlbp,
	Tlbwi,
	Tlbwr,
	Wait,
	Swr,
	Xor,
	Xori,
}

#[allow(unused_variables)]
pub fn decode_instr(instr: u32, addr: Addr, uarch_info: &UarchInfo) {
}

pub struct MipsDisasm;

impl Disassembler for MipsDisasm {
	#[allow(unused_variables)]
	fn disassemble(&self, addr: Addr, buf: &[u8]) -> DisResult  {
		if buf.len() < 4 {
			return Err(DisError::MemOverflow);
		}

		if (addr % 4) != 0 {
			return Err( DisError::Unaligned{ desired_alignment: 4, } );
		}

		Err(DisError::Unknown{num_bytes: 4})
	}

	fn bytes_per_unit(&self) -> u16 {
		4
	}
}


/* ============================================================================
 * File:   detail.rs
 * Author: Cole Johnson
 * ============================================================================
 * Copyright (c) 2020 Cole Johnson
 *
 * This file is part of hasm.
 *
 * hasm is free software: you can redistribute it and/or modify it under the
 *   terms of the GNU General Public License as published by the Free Software
 *   Foundation, either version 3 of the License, or (at your option) any later
 *   version.
 *
 * hasm is distributed in the hope that it will be useful, but WITHOUT ANY
 *   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *   FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 *   details.
 *
 * You should have received a copy of the GNU General Public License along with
 *   hasm. If not, see <http://www.gnu.org/licenses/>.
 * ============================================================================
 */
pub struct Instruction {
    pub opcode: String,
    pub operands: [Option<OperandWithSeg>; 4],
}

pub struct OperandWithSeg {
    pub seg: Option<Register>,
    pub oper: Operand,
}

pub enum Operand {
    Indirect(Indirect),
    Reg(Register),
    Imm(u64),
}

pub struct Indirect {
    pub base: Option<Register>,
    pub reg: Option<Register>,
    pub scaled_reg: Option<Register>,
    pub scale: u8,
    pub displacement: i32, // TODO: u64?
}

pub struct Register {
    pub bits_width: u8,
    pub reg: RegisterType,
    pub flags: Option<AvxFlags>,
    // k0..7; {z}
    pub mask: Option<(u8, bool)>,
}

pub enum AvxFlags {
    EmbeddedRounding,     // {er} (implies {sae})
    SupressAllExceptions, // {sae}
}

pub enum RegisterType {
    /* In order (note rbx is NOT idx 1)
     * 0 - rax: accumulator
     * 1 - rcx: counter
     * 2 - rdx: data
     * 3 - rbx: base
     * 4 - rsp: stack pointer
     * 5 - rbp: base pointer
     * 6 - rsi: source index
     * 7 - rdi: desination index
     * 8 - r8-r15: x86-64 extended registers
     *
     * NOTE: In long mode, the order for
     *   8-bit is (based on REX):
     *     w/    w/o
     * 0 - al  | al
     * 1 - cl  | cl
     * 2 - dl  | dl
     * 3 - bl  | bl
     * 4 - spl | ah
     * 5 - bpl | ch
     * 6 - sil | dh
     * 7 - dil | bh
     */
    GeneralPurpose(u8),
    // rdx:rax
    RdxRaxPair,
    /* In order: (TODO: correct?)
     * 0 - cs: code
     * 1 - ds: data
     * 2 - ss: stack
     * 3 - es: data
     * 4 - fs: data
     * 5 - gs: data
     */
    Segment(u8),
    // RFLAGS
    Flags,
    // ST0..7
    FloatingPoint(u8),
    // CR0..15
    Control(u8),
    // DR0..15
    Debug(u8),
    // BND0..3
    Bound(u8),
    // MM0..7
    Mmx(u8),
    // XMM0..31
    Xmm(u8),
    // YMM0..31
    Ymm(u8),
    // ZMM0..31
    Zmm(u8),
    // VR0..15
    Vsib(u8),
    // TODO: BNDCFGU and BNDSTATUS?
    // TODO: XCR0?
    // TODO: CET registers?
}

pub struct Directive {
    pub directive: String,
    // TODO: values
}

pub struct AsmLine {
    pub label: Option<String>,
    pub instr: Option<Instruction>,
    pub comment: Option<String>,
}

pub struct MacroLine {}

pub struct DirectiveLine {
    pub directive: String,
    pub args: Vec<String>,
}

pub enum Line {
    Asm(AsmLine),
    Macro(MacroLine),
    Directive(DirectiveLine),
}

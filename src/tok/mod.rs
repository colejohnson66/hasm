/* ============================================================================
 * File:   mod.rs
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
mod detail;

use crate::io::iter::LineOffsetIterator;
pub use detail::*;
use std::io::Cursor;

pub struct AsmTokenizer {
    asm: LineOffsetIterator,
}

impl AsmTokenizer {
    pub fn new(asm: &str) -> AsmTokenizer {
        let mut cursor = Cursor::new(asm);
        AsmTokenizer {
            asm: LineOffsetIterator::new(&mut cursor),
        }
    }

    // In the impl blocks below, each fn MUST reset the state
    //   if nothing matches.
    // The return value is an Option<T> struct with two states:
    //  - None => no match
    //  - Some(T) => a match; T may or may not contain data
}

impl AsmTokenizer {
    fn read_line(&mut self) -> Option<Line> {
        // <Line> ::
        //     <AsmLine>
        //     <MacroLine>
        //     <DirectiveLine>
        //     epsilon

        let state = self.asm.state();

        // match <AsmLine>
        match self.asm_line() {
            Some(tok) => return Some(Line::Asm(tok)),
            None => (),
        }

        // match <MacroLine>
        match self.macro_line() {
            Some(tok) => return Some(Line::Macro(tok)),
            None => (),
        }

        // match <DirectiveLine>
        match self.directive_line() {
            Some(tok) => return Some(Line::Directive(tok)),
            None => (),
        }

        // don't reset state; return epsilon
        None
    }

    fn asm_line(&mut self) -> Option<AsmLine> {
        // <AsmLine> ::
        //     opt<Label> opt<Instruction> opt<Comment>

        let state = self.asm.state();

        // match opt<Label>
        let label = self.label();

        // match opt<Instruction>
        let instr = self.instruction();

        // match opt<Comment>
        let comment = self.comment();

        if label.is_none() && instr.is_none() && comment.is_none() {
            self.asm.set_state(state);
            return None;
        }

        Some(AsmLine {
            label,
            instr,
            comment,
        })
    }

    fn label(&mut self) -> Option<String> {
        // <Label> ::
        //     rep<LabelChar> ": "
        // <LabelChars> ::
        //     range['a'-'z']
        //     range['A'-'Z']
        //     range['0'-'9']
        //     oneOf['_', '$', '@']

        let state = self.asm.state();

        // match rep<LabelChar>
        let mut val: String = "".into();
        loop {
            match self.asm.peek() {
                Some(c) => {
                    if c.is_ascii_alphanumeric() {
                        self.asm.consume();
                        val.push(c);
                    } else if c == '_' || c == '$' || c == '@' {
                        self.asm.consume();
                        val.push(c);
                    } else {
                        break;
                    }
                }
                None => break,
            }
        }

        if val.len() == 0 {
            return None;
        }

        // match ": "
        match self.asm.read() {
            Some(':') => match self.asm.read() {
                Some(' ') => return Some(val),
                _ => (),
            },
            _ => (),
        }

        self.asm.set_state(state);
        None
    }

    fn instruction(&mut self) -> Option<Instruction> {
        // <Instruction> ::
        //     <Opcode>
        //     <Opcode> " " optrep[" "] <Operand>
        //     <Opcode> " " optrep[" "] <Operand> optrep[" "] "," optrep[" "] <Operand>
        //     <Opcode> " " optrep[" "] <Operand> optrep[" "] "," optrep[" "] <Operand> optrep[" "] "," optrep[" "] <Operand>
        //     <Opcode> " " optrep[" "] <Operand> optrep[" "] "," optrep[" "] <Operand> optrep[" "] "," optrep[" "] <Operand> optrep[" "] "," optrep[" "] <Operand>

        // TODO: allow directives (such as `db` too)

        let opcode = self.opcode();
        if opcode.is_none() {
            return None;
        }
        let opcode = opcode.unwrap();

        // match " "
        match self.asm.peek() {
            Some(' ') => self.asm.consume(),
            _ => {
                return Some(Instruction {
                    opcode,
                    operands: vec![],
                });
            }
        }

        let mut operands: Vec<OperandWithSeg> = Vec::with_capacity(8);

        // operand 1
        let state = self.asm.state();
        // match optrep[" "]
        self.asm.consume_all(' ');
        // match <Operand>
        match self.operand() {
            Some(operand) => operands.push(operand),
            None => {
                self.asm.set_state(state);
                return Some(Instruction { opcode, operands });
            }
        }

        // operand 2
        let state = self.asm.state();
        // match optrep[" "]
        self.asm.consume_all(' ');
        // match ","
        match self.asm.peek() {
            Some(',') => self.asm.consume(),
            _ => {
                self.asm.set_state(state);
                return Some(Instruction { opcode, operands });
            }
        }
        // match optrep[" "]
        self.asm.consume_all(' ');
        // match <Operand>
        match self.operand() {
            Some(operand) => operands.push(operand),
            None => {
                self.asm.set_state(state);
                return Some(Instruction { opcode, operands });
            }
        }

        // operand 3
        let state = self.asm.state();
        // match optrep[" "]
        self.asm.consume_all(' ');
        // match ","
        match self.asm.peek() {
            Some(',') => self.asm.consume(),
            _ => {
                self.asm.set_state(state);
                return Some(Instruction { opcode, operands });
            }
        }
        // match optrep[" "]
        self.asm.consume_all(' ');
        // match <Operand>
        match self.operand() {
            Some(operand) => operands.push(operand),
            None => {
                self.asm.set_state(state);
                return Some(Instruction { opcode, operands });
            }
        }

        // operand 4
        let state = self.asm.state();
        // match optrep[" "]
        self.asm.consume_all(' ');
        // match ","
        match self.asm.peek() {
            Some(',') => self.asm.consume(),
            _ => {
                self.asm.set_state(state);
                return Some(Instruction { opcode, operands });
            }
        }
        // match optrep[" "]
        self.asm.consume_all(' ');
        // match <Operand>
        match self.operand() {
            Some(operand) => operands.push(operand),
            None => {
                self.asm.set_state(state);
                return Some(Instruction { opcode, operands });
            }
        }

        Some(Instruction { opcode, operands })
    }

    fn opcode(&mut self) -> Option<String> {
        // <Opcode> ::
        //     rep<OpcodeChars>
        // <OpcodeChars> ::
        //     range['a'-'z']
        //     range['A'-'Z']
        //     range['0'-'9']
        // NOTE: <Opcode> is case insensitive; return all lowercase

        let state = self.asm.state();

        // match rep<LabelChar>
        let mut val: String = "".into();
        loop {
            match self.asm.peek() {
                Some(c) => {
                    if c.is_ascii_alphanumeric() {
                        self.asm.consume();
                        val.push(c.to_ascii_lowercase());
                    } else {
                        break;
                    }
                }
                None => break,
            }
        }

        if val.len() == 0 {
            return None;
        }

        Some(val)
    }

    fn operand(&mut self) -> Option<OperandWithSeg> {
        // <Operand> ::
        //     opt<SegmentPrefix> <Indirect>
        //     opt<SegmentPrefix> <Register>
        //     opt<SegmentPrefix> <Immediate>

        let state = self.asm.state();

        // match opt<SegmentPrefix>
        let seg = self.segment_prefix();

        // match <Indirect>
        match self.indirect() {
            Some(indirect) => {
                return Some(OperandWithSeg {
                    seg,
                    oper: Operand::Indirect(indirect),
                })
            }
            None => (),
        }

        // match <Register>
        match self.register() {
            Some(reg) => {
                return Some(OperandWithSeg {
                    seg,
                    oper: Operand::Reg(reg),
                })
            }
            None => (),
        }

        // match <Immediate>
        match self.immediate() {
            Some(imm) => {
                return Some(OperandWithSeg {
                    seg,
                    oper: Operand::Imm(imm),
                })
            }
            None => (),
        }

        self.asm.set_state(state);
        None
    }

    fn segment_prefix(&mut self) -> Option<Register> {
        // TODO: use <SegmentRegister>
        // <SegmentPrefix> ::
        //     <Segment> optrep[" "] ":" optrep[" "]
        // <Segment> ::
        //     caseinsensitive["cs"] // 0
        //     caseinsensitive["ds"] // 1
        //     caseinsensitive["ss"] // 2
        //     caseinsensitive["es"] // 3
        //     caseinsensitive["fs"] // 4
        //     caseinsensitive["gs"] // 5

        let state = self.asm.state();

        // match <Segment>
        let mut buf = ['\0'; 2];
        if self.asm.read_multiple(&mut buf[..]) != 2 {
            self.asm.set_state(state);
            return None;
        }
        let seg: Option<u8> = match &buf.iter().collect::<String>().to_ascii_lowercase()[..] {
            "cs" => Some(0),
            "ds" => Some(1),
            "ss" => Some(2),
            "es" => Some(3),
            "fs" => Some(4),
            "gs" => Some(5),
            _ => None,
        };
        if seg.is_none() {
            self.asm.set_state(state);
            return None;
        }
        let seg = RegisterType::Segment(seg.unwrap());

        // match optrep[" "]
        loop {
            match self.asm.read() {
                Some(' ') => continue,
                _ => break,
            }
        }

        // match ":"
        match self.asm.read() {
            Some(':') => (),
            _ => {
                self.asm.set_state(state);
                return None;
            }
        }

        // match optrep[" "]
        loop {
            match self.asm.read() {
                Some(' ') => continue,
                _ => break,
            }
        }

        Some(Register {
            bit_width: 0,
            reg: seg,
            flags: None,
            mask: None,
        })
    }

    fn indirect(&mut self) -> Option<Indirect> {
        // NOTE: <Indirect> takes many forms; Ultimately, it is an expression
        //   of the form  `[base+index*scale+disp]` where each value is
        //   optional, but at least one register must be present. In addition,
        //   optional whitespace may exist between each "block" (it is not
        //   present here for readability).
        // TODO: "[" <Register> "+" <Register> "+" <Number> "]" // Implied scale of 1
        // TODO: This is not exhaustive
        // <Indirect> ::
        //     "[" <Register> "]"
        //     "[" <Register> "+" <Register> "]"
        //     "[" <Register> "+" <Register> "*" <Scale> "]"
        //     "[" <Register> "+" <Register> "*" <Scale> "+" <Number> "]"
        // <Scale> ::
        //     oneOf["1", "2", "4", "8"]

        let state = self.asm.state();

        // match "["
        match self.asm.peek() {
            Some('[') => self.asm.consume(),
            _ => return None,
        }

        // match optrep[" "]
        self.asm.consume_all(' ');

        // match <Register>
        let base = self.register();
        if base.is_none() {
            self.asm.set_state(state);
            return None;
        }

        // match optrep[" "]
        self.asm.consume_all(' ');

        // match "]" or "+"
        match self.asm.peek() {
            Some(']') => {
                return Some(Indirect {
                    base,
                    index: None,
                    scale: 0,
                    disp: 0,
                })
            }
            Some('+') => (),
            _ => {
                self.asm.set_state(state);
                return None;
            }
        }

        // match optrep[" "]
        self.asm.consume_all(' ');

        // match <Register>
        let index = self.register();
        if index.is_none() {
            self.asm.set_state(state);
            return None;
        }

        // match optrep[" "]
        self.asm.consume_all(' ');

        // match "]" or "*"
        match self.asm.peek() {
            Some(']') => {
                return Some(Indirect {
                    base,
                    index,
                    scale: 1, // implied
                    disp: 0,
                });
            }
            Some('*') => (),
            _ => {
                self.asm.set_state(state);
                return None;
            }
        }

        // match optrep[" "]
        self.asm.consume_all(' ');

        // match <Scale>
        let scale = match self.asm.peek() {
            Some('1') => 1u8,
            Some('2') => 2u8,
            Some('4') => 4u8,
            Some('8') => 8u8,
            _ => {
                self.asm.set_state(state);
                return None;
            }
        };

        // match optrep[" "]
        self.asm.consume_all(' ');

        // match "]" or "+"
        match self.asm.peek() {
            Some(']') => {
                return Some(Indirect {
                    base,
                    index,
                    scale,
                    disp: 0,
                });
            }
            Some('+') => (),
            _ => {
                self.asm.set_state(state);
                return None;
            }
        }

        // match optrep[" "]
        self.asm.consume_all(' ');

        // match <Number>
        let disp = self.i32();
        if disp.is_none() {
            self.asm.set_state(state);
            return None;
        }
        let disp = disp.unwrap();

        // match optrep[" "]
        self.asm.consume_all(' ');

        // match "]"
        match self.asm.peek() {
            Some(']') => (),
            _ => {
                self.asm.set_state(state);
                return None;
            }
        }

        Some(Indirect {
            base,
            index,
            scale,
            disp,
        })
    }

    fn register(&mut self) -> Option<Register> {
        // <Register> ::
        //     <GeneralPurposeRegister>
        //     // TODO: rDX:rAX pair
        //     <SegmentRegister>
        //     <FlagsRegister>
        //     <FloatingPointRegister>
        //     <ControlRegister>
        //     <DebugRegister>
        //     <BoundRegister>
        //     <MmxRegister>
        //     <AvxRegister>
        //     <VsibRegister>
        unimplemented!();
    }

    fn general_purpose_register(&mut self) -> Option<Register> {
        // All case insensitive; for clarity, `caseInsensitive[...]` is omitted
        // <GeneralPurposeRegister> ::
        //     oneOf["al", "ah", "ax", "eax", "rax"]
        //     oneOf["cl", "ch", "cx", "ecx", "rcx"]
        //     oneOf["dl", "dh", "dx", "edx", "rdx"]
        //     oneOf["bl", "bh", "bx", "ebx", "rbx"]
        //     oneOf["spl", "sph", "sp", "esp", "rsp"]
        //     oneOf["bpl", "bph", "bp", "ebp", "rbp"]
        //     oneOf["sil", "sih", "si", "esi", "rsi"]
        //     oneOf["dil", "dih", "di", "edi", "rdi"]
        //     oneOf["r8b", "r8w", "r8d", "r8"]
        //     oneOf["r9b", "r9w", "r9d", "r9"]
        //     oneOf["r10b", "r10w", "r10d", "r10"]
        //     oneOf["r11b", "r11w", "r11d", "r11"]
        //     oneOf["r12b", "r12w", "r12d", "r12"]
        //     oneOf["r13b", "r13w", "r13d", "r13"]
        //     oneOf["r14b", "r14w", "r14d", "r14"]
        //     oneOf["r15b", "r15w", "r15d", "r15"]
        unimplemented!();
    }

    fn segment_register(&mut self) -> Option<Register> {
        // <SegmentRegister> ::
        //     caseInsensitive["cs"]
        //     caseInsensitive["ds"]
        //     caseInsensitive["ss"]
        //     caseInsensitive["es"]
        //     caseInsensitive["fs"]
        //     caseInsensitive["gs"]

        let state = self.asm.state();

        let mut buf = ['\0'; 2];
        if self.asm.read_multiple(&mut buf) != 2 {
            self.asm.set_state(state);
            return None;
        }
        let buf = buf.iter().collect::<String>().to_ascii_lowercase();

        let seg: Option<u8> = match &buf[..] {
            "cs" => Some(0),
            "ds" => Some(1),
            "ss" => Some(2),
            "es" => Some(3),
            "fs" => Some(4),
            "gs" => Some(5),
            _ => None,
        };
        if seg.is_none() {
            self.asm.set_state(state);
            return None;
        }

        Some(Register {
            bit_width: 0,
            reg: RegisterType::Segment(seg.unwrap()),
            flags: None,
            mask: None,
        })
    }

    fn flags_register(&mut self) -> Option<Register> {
        // <FlagsRegister> ::
        //     caseInsensitive["flags"]
        //     caseInsensitive["eflags"]
        //     caseInsensitive["rflags"]

        let state = self.asm.state();

        let mut buf = ['\0'; 5];
        if self.asm.read_multiple(&mut buf[..]) != 5 {
            self.asm.set_state(state);
            return None;
        }
        let buf = buf.iter().collect::<String>().to_ascii_lowercase();
        let size: Option<u8> = match &buf[..] {
            "flags" => Some(16),
            "eflag" => match self.asm.read() {
                Some('s') => Some(32),
                _ => None,
            },
            "rflag" => match self.asm.read() {
                Some('s') => Some(64),
                _ => None,
            },
            _ => None,
        };
        if size.is_none() {
            self.asm.set_state(state);
            return None;
        }

        Some(Register {
            bit_width: size.unwrap(),
            reg: RegisterType::Flags,
            flags: None,
            mask: None,
        })
    }

    fn floating_point_register(&mut self) -> Option<Register> {
        // <FloatingPointRegister> ::
        //     caseInsensitive["st"] "(" range["0"-"7"] ")"
        //     caseInsensitive["st"] range["0"-"7"]

        let state = self.asm.state();

        let mut buf = ['\0'; 2];
        if self.asm.read_multiple(&mut buf) != 2 {
            self.asm.set_state(state);
            return None;
        }
        let buf = buf.iter().collect::<String>().to_ascii_lowercase();
        if &buf[..] != "st" {
            self.asm.set_state(state);
            return None;
        }

        // match "("
        let paren = match self.asm.peek() {
            Some('(') => {
                self.asm.consume();
                true
            }
            _ => false,
        };

        // match range["0"-"7"]
        let reg: u8 = match self.asm.read() {
            Some('0') => 0,
            Some('1') => 1,
            Some('2') => 2,
            Some('3') => 3,
            Some('4') => 4,
            Some('5') => 5,
            Some('6') => 6,
            Some('7') => 7,
            _ => {
                self.asm.set_state(state);
                return None;
            }
        };

        // match ")" if needed
        if paren {
            match self.asm.read() {
                Some(')') => (),
                _ => {
                    self.asm.set_state(state);
                    return None;
                }
            }
        }

        Some(Register {
            bit_width: 0,
            reg: RegisterType::FloatingPoint(reg),
            flags: None,
            mask: None,
        })
    }

    fn control_register(&mut self) -> Option<Register> {
        // <ControlRegister> ::
        //     caseInsensitive["cr"] range["0"-"15"]

        let state = self.asm.state();

        // match caseInsensitive["cr"]
        let mut buf = ['\0'; 2];
        if self.asm.read_multiple(&mut buf) != 2 {
            self.asm.set_state(state);
            return None;
        }
        let buf = buf.iter().collect::<String>().to_ascii_lowercase();
        if &buf[..] != "cr" {
            self.asm.set_state(state);
            return None;
        }

        // match range["0"-"15"]
        let reg_num = match self.asm.read() {
            Some('0') => Some(0),
            Some('1') => match self.asm.peek() {
                Some(c) if c >= '0' && c <= '5' => {
                    self.asm.read();
                    let digit = c as u8 - '0' as u8;
                    Some(10 + digit)
                }
                _ => Some(1),
            },
            Some(c) if c >= '2' && c <= '9' => {
                let digit = c as u8 - '0' as u8;
                Some(digit)
            }
            _ => None,
        };
        if reg_num.is_none() {
            self.asm.set_state(state);
            return None;
        }

        Some(Register {
            bit_width: 0,
            reg: RegisterType::Control(reg_num.unwrap()),
            flags: None,
            mask: None,
        })
    }

    fn debug_register(&mut self) -> Option<Register> {
        // <ControlRegister> ::
        //     caseInsensitive["dr"] range["0"-"15"]

        let state = self.asm.state();

        // match caseInsensitive["dr"]
        let mut buf = ['\0'; 2];
        if self.asm.read_multiple(&mut buf) != 2 {
            self.asm.set_state(state);
            return None;
        }
        let buf = buf.iter().collect::<String>().to_ascii_lowercase();
        if &buf[..] != "dr" {
            self.asm.set_state(state);
            return None;
        }

        // match range["0"-"15"]
        let reg_num = match self.asm.read() {
            Some('0') => Some(0),
            Some('1') => match self.asm.peek() {
                Some(c) if c >= '0' && c <= '5' => {
                    self.asm.read();
                    let digit = c as u8 - '0' as u8;
                    Some(10 + digit)
                }
                _ => Some(1),
            },
            Some(c) if c >= '2' && c <= '9' => {
                let digit = c as u8 - '0' as u8;
                Some(digit)
            }
            _ => None,
        };
        if reg_num.is_none() {
            self.asm.set_state(state);
            return None;
        }

        Some(Register {
            bit_width: 0,
            reg: RegisterType::Debug(reg_num.unwrap()),
            flags: None,
            mask: None,
        })
    }

    fn bound_register(&mut self) -> Option<Register> {
        // <BoundRegister> ::
        //     caseInsensitive["bnd"] range["0"-"3"]

        let state = self.asm.state();

        // match caseInsensitive["bnd"]
        let mut buf = ['\0'; 3];
        if self.asm.read_multiple(&mut buf) != 3 {
            self.asm.set_state(state);
            return None;
        }
        let buf = buf.iter().collect::<String>().to_ascii_lowercase();
        if &buf[..] != "bnd" {
            self.asm.set_state(state);
            return None;
        }

        // match range["0"-"3"]
        let reg_num: Option<u8> = match self.asm.read() {
            Some('0') => Some(0),
            Some('1') => Some(1),
            Some('2') => Some(2),
            Some('3') => Some(3),
            _ => None,
        };
        if reg_num.is_none() {
            self.asm.set_state(state);
            return None;
        }

        Some(Register {
            bit_width: 0,
            reg: RegisterType::Bound(reg_num.unwrap()),
            flags: None,
            mask: None,
        })
    }

    fn mmx_register(&mut self) -> Option<Register> {
        // <MmxRegister> ::
        //     caseInsensitive["mm"] range["0"-"7"]

        let state = self.asm.state();

        // match caseInsensitive["mm"]
        let mut buf = ['\0'; 2];
        if self.asm.read_multiple(&mut buf) != 2 {
            self.asm.set_state(state);
            return None;
        }
        let buf = buf.iter().collect::<String>().to_ascii_lowercase();
        if &buf[..] != "mm" {
            self.asm.set_state(state);
            return None;
        }

        // match range["0"-"3"]
        let reg_num: Option<u8> = match self.asm.read() {
            Some('0') => Some(0),
            Some('1') => Some(1),
            Some('2') => Some(2),
            Some('3') => Some(3),
            Some('4') => Some(4),
            Some('5') => Some(5),
            Some('6') => Some(6),
            Some('7') => Some(7),
            _ => None,
        };
        if reg_num.is_none() {
            self.asm.set_state(state);
            return None;
        }

        Some(Register {
            bit_width: 0,
            reg: RegisterType::Mmx(reg_num.unwrap()),
            flags: None,
            mask: None,
        })
    }

    fn avx_register(&mut self) -> Option<Register> {
        unimplemented!();
    }

    fn vsib_register(&mut self) -> Option<Register> {
        unimplemented!();
    }

    fn immediate(&mut self) -> Option<i64> {
        // <Immediate> ::
        //      <i64>
        unimplemented!();
    }

    fn comment(&mut self) -> Option<String> {
        unimplemented!();
    }

    fn macro_line(&mut self) -> Option<MacroLine> {
        unimplemented!();
    }

    fn directive_line(&mut self) -> Option<DirectiveLine> {
        unimplemented!();
    }

    fn i32(&mut self) -> Option<i32> {
        unimplemented!();
    }

    fn i64(&mut self) -> Option<i64> {
        unimplemented!();
    }
}

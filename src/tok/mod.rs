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
        unimplemented!();
    }

    fn register(&mut self) -> Option<Register> {
        unimplemented!();
    }

    fn immediate(&mut self) -> Option<i64> {
        // <Immediate> ::
        //      <Number>
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
}

/**********************************************************************************
 * dcc6502.c -> Main module of:                                                   *
 * Disassembler and Cycle Counter for the 6502 microprocessor                     *
 *                                                                                *
 * This code is offered under the MIT License (MIT)                               *
 *                                                                                *
 * Copyright (c) 2014 Tennessee Carmel-Veilleux <veilleux@tentech.ca              *
 *                                                                                *
 * Permission is hereby granted, free of charge, to any person obtaining a copy   *
 * of this software and associated documentation files (the "Software"), to deal  *
 * in the Software without restriction, including without limitation the rights   *
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *
 * copies of the Software, and to permit persons to whom the Software is          *
 * furnished to do so, subject to the following conditions:                       *
 *                                                                                *
 * The above copyright notice and this permission notice shall be included in all *
 * copies or substantial portions of the Software.                                *
 *                                                                                *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *
 * SOFTWARE.                                                                      *
 **********************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define VERSION_INFO "v1.6"
#define NUMBER_OPCODES 151

/* The 6502's 13 addressing modes */
#define IMMED 0 /* Immediate */
#define ABSOL 1 /* Absolute */
#define ZEROP 2 /* Zero Page */
#define IMPLI 3 /* Implied */
#define INDIA 4 /* Indirect Absolute */
#define ABSIX 5 /* Absolute indexed with X */
#define ABSIY 6 /* Absolute indexed with Y */
#define ZEPIX 7 /* Zero page indexed with X */
#define ZEPIY 8 /* Zero page indexed with Y */
#define INDIN 9 /* Indexed indirect (with x) */
#define ININD 10 /* Indirect indexed (with y) */
#define RELAT 11 /* Relative */
#define ACCUM 12 /* Accumulator */

#define LSB_FIRST

typedef struct OPcode {
    unsigned char number; /* Number of the opcode */
    unsigned char name; /* Index in the name table */
    unsigned char addressing; /* Addressing mode */
    unsigned char cycles; /* Number of cycles */
    unsigned char cross_page; /* 1 if cross-page boundaries affect cycles */
} OPcode;

// FIXME: Stop using the needless union and become native-endianness-agnostic
typedef union
{
#ifdef LSB_FIRST
    struct { unsigned char l, h; } B;
#else
    struct { byte h, l; } B;
#endif
    unsigned short W;
} word;

char name_table[56][4] = {
    "ADC", "AND", "ASL", "BCC", "BCS", "BEQ", "BIT", "BMI", "BNE", "BPL",
    "BRK", "BVC", "BVS", "CLC", "CLD", "CLI", "CLV", "CMP", "CPX", "CPY",
    "DEC", "DEX", "DEY", "EOR", "INC", "INX", "INY", "JMP", "JSR", "LDA",
    "LDX", "LDY", "LSR", "NOP", "ORA", "PHA", "PHP", "PLA", "PLP", "ROL",
    "ROR", "RTI", "RTS", "SBC", "SEC", "SED", "SEI", "STA", "STX", "STY",
    "TAX", "TAY", "TSX", "TXA", "TXS", "TYA"
};

/* Opcode table */
OPcode opcode_table[NUMBER_OPCODES] = {
    {0x69, 0, IMMED, 2, 1}, /* ADC */
    {0x65, 0, ZEROP, 3, 1},
    {0x75, 0, ZEPIX, 4, 1},
    {0x6D, 0, ABSOL, 4, 1},
    {0x7D, 0, ABSIX, 4, 1},
    {0x79, 0, ABSIY, 4, 1},
    {0x61, 0, INDIN, 6, 1},
    {0x71, 0, ININD, 5, 1},

    {0x29, 1, IMMED, 2, 1}, /* AND */
    {0x25, 1, ZEROP, 3, 1},
    {0x35, 1, ZEPIX, 4, 1},
    {0x2D, 1, ABSOL, 4, 1},
    {0x3D, 1, ABSIX, 4, 1},
    {0x39, 1, ABSIY, 4, 1},
    {0x21, 1, INDIN, 6, 1},
    {0x31, 1, ININD, 5, 1},

    {0x0A, 2, ACCUM, 2, 0}, /* ASL */
    {0x06, 2, ZEROP, 5, 0},
    {0x16, 2, ZEPIX, 6, 0},
    {0x0E, 2, ABSOL, 6, 0},
    {0x1E, 2, ABSIX, 6, 0},

    {0x90, 3, RELAT, 4, 1}, /* BCC */

    {0xB0, 4, RELAT, 4, 1}, /* BCS */

    {0xF0, 5, RELAT, 4, 1}, /* BEQ */

    {0x24, 6, ZEROP, 3, 0}, /* BIT */
    {0x2C, 6, ABSOL, 4, 0},

    {0x30, 7, RELAT, 4, 1}, /* BMI */

    {0xD0, 8, RELAT, 4, 1}, /* BNE */

    {0x10, 9, RELAT, 4, 1}, /* BPL */

    {0x00, 10, IMPLI, 7, 0}, /* BRK */

    {0x50, 11, RELAT, 4, 1}, /* BVC */

    {0x70, 12, RELAT, 4, 1}, /* BVS */

    {0x18, 13, IMPLI, 2, 0}, /* CLC */

    {0xD8, 14, IMPLI, 2, 0}, /* CLD */

    {0x58, 15, IMPLI, 2, 0}, /* CLI */

    {0xB8, 16, IMPLI, 2, 0}, /* CLV */

    {0xC9, 17, IMMED, 2, 0}, /* CMP */
    {0xC5, 17, ZEROP, 3, 0},
    {0xD5, 17, ZEPIX, 4, 0},
    {0xCD, 17, ABSOL, 4, 0},
    {0xDD, 17, ABSIX, 4, 0},
    {0xD9, 17, ABSIY, 4, 0},
    {0xC1, 17, INDIN, 6, 0},
    {0xD1, 17, ININD, 5, 0},

    {0xE0, 18, IMMED, 2, 0}, /* CPX */
    {0xE4, 18, ZEROP, 3, 0},
    {0xEC, 18, ABSOL, 4, 0},

    {0xC0, 19, IMMED, 2, 0}, /* CPY */
    {0xC4, 19, ZEROP, 3, 0},
    {0xCC, 19, ABSOL, 4, 0},

    {0xC6, 20, ZEROP, 5, 0}, /* DEC */
    {0xD6, 20, ZEPIX, 6, 0},
    {0xCE, 20, ABSOL, 6, 0},
    {0xDE, 20, ABSIX, 6, 0},

    {0xCA, 21, IMPLI, 2, 0}, /* DEX */

    {0x88, 22, IMPLI, 2, 0}, /* DEY */

    {0x49, 23, IMMED, 2, 1}, /* EOR */
    {0x45, 23, ZEROP, 3, 1},
    {0x55, 23, ZEPIX, 4, 1},
    {0x4D, 23, ABSOL, 4, 1},
    {0x5D, 23, ABSIX, 4, 1},
    {0x59, 23, ABSIY, 4, 1},
    {0x41, 23, INDIN, 6, 1},
    {0x51, 23, ININD, 5, 1},

    {0xE6, 24, ZEROP, 5, 0}, /* INC */
    {0xF6, 24, ZEPIX, 6, 0},
    {0xEE, 24, ABSOL, 6, 0},
    {0xFE, 24, ABSIX, 6, 0},

    {0xE8, 25, IMPLI, 2, 0}, /* INX */

    {0xC8, 26, IMPLI, 2, 0}, /* INY */

    {0x4C, 27, ABSOL, 3, 0}, /* JMP */
    {0x6C, 27, INDIA, 5, 0},

    {0x20, 28, ABSOL, 6, 0}, /* JSR */

    {0xA9, 29, IMMED, 2, 1}, /* LDA */
    {0xA5, 29, ZEROP, 3, 1},
    {0xB5, 29, ZEPIX, 4, 1},
    {0xAD, 29, ABSOL, 4, 1},
    {0xBD, 29, ABSIX, 4, 1},
    {0xB9, 29, ABSIY, 4, 1},
    {0xA1, 29, INDIN, 6, 1},
    {0xB1, 29, ININD, 5, 1},

    {0xA2, 30, IMMED, 2, 1}, /* LDX */
    {0xA6, 30, ZEROP, 3, 1},
    {0xB6, 30, ZEPIY, 4, 1},
    {0xAE, 30, ABSOL, 4, 1},
    {0xBE, 30, ABSIY, 4, 1},

    {0xA0, 31, IMMED, 2, 1}, /* LDY */
    {0xA4, 31, ZEROP, 3, 1},
    {0xB4, 31, ZEPIX, 4, 1},
    {0xAC, 31, ABSOL, 4, 1},
    {0xBC, 31, ABSIX, 4, 1},

    {0x4A, 32, ACCUM, 2, 0}, /* LSR */
    {0x46, 32, ZEROP, 5, 0},
    {0x56, 32, ZEPIX, 6, 0},
    {0x4E, 32, ABSOL, 6, 0},
    {0x5E, 32, ABSIX, 6, 0},

    {0xEA, 33, IMPLI, 2, 0}, /* NOP */

    {0x09, 34, IMMED, 2, 0}, /* ORA */
    {0x05, 34, ZEROP, 3, 0},
    {0x15, 34, ZEPIX, 4, 0},
    {0x0D, 34, ABSOL, 4, 0},
    {0x1D, 34, ABSIX, 4, 0},
    {0x19, 34, ABSIY, 4, 0},
    {0x01, 34, INDIN, 6, 0},
    {0x11, 34, ININD, 5, 0},

    {0x48, 35, IMPLI, 3, 0}, /* PHA */

    {0x08, 36, IMPLI, 3, 0}, /* PHP */

    {0x68, 37, IMPLI, 4, 0}, /* PLA */

    {0x28, 38, IMPLI, 4, 0}, /* PLP */

    {0x2A, 39, ACCUM, 2, 0}, /* ROL */
    {0x26, 39, ZEROP, 5, 0},
    {0x36, 39, ZEPIX, 6, 0},
    {0x2E, 39, ABSOL, 6, 0},
    {0x3E, 39, ABSIX, 6, 0},

    {0x6A, 40, ACCUM, 2, 0}, /* ROR */
    {0x66, 40, ZEROP, 5, 0},
    {0x76, 40, ZEPIX, 6, 0},
    {0x6E, 40, ABSOL, 6, 0},
    {0x7E, 40, ABSIX, 6, 0},

    {0x40, 41, IMPLI, 6, 0}, /* RTI */

    {0x60, 42, IMPLI, 6, 0}, /* RTS */

    {0xE9, 43, IMMED, 2, 1}, /* SBC */
    {0xE5, 43, ZEROP, 3, 1},
    {0xF5, 43, ZEPIX, 4, 1},
    {0xED, 43, ABSOL, 4, 1},
    {0xFD, 43, ABSIX, 4, 1},
    {0xF9, 43, ABSIY, 4, 1},
    {0xE1, 43, INDIN, 6, 1},
    {0xF1, 43, ININD, 5, 1},

    {0x38, 44, IMPLI, 2, 0}, /* SEC */

    {0xF8, 45, IMPLI, 2, 0}, /* SED */

    {0x78, 46, IMPLI, 2, 0}, /* SEI */

    {0x85, 47, ZEROP, 3, 0}, /* STA */
    {0x95, 47, ZEPIX, 4, 0},
    {0x8D, 47, ABSOL, 4, 0},
    {0x9D, 47, ABSIX, 4, 0},
    {0x99, 47, ABSIY, 4, 0},
    {0x81, 47, INDIN, 6, 0},
    {0x91, 47, ININD, 5, 0},

    {0x86, 48, ZEROP, 3, 0}, /* STX */
    {0x96, 48, ZEPIY, 4, 0},
    {0x8E, 48, ABSOL, 4, 0},

    {0x84, 49, ZEROP, 3, 0}, /* STY */
    {0x94, 49, ZEPIX, 4, 0},
    {0x8C, 49, ABSOL, 4, 0},

    {0xAA, 50, IMPLI, 2, 0}, /* TAX */

    {0xA8, 51, IMPLI, 2, 0}, /* TAY */

    {0xBA, 52, IMPLI, 2, 0}, /* TSX */

    {0x8A, 53, IMPLI, 2, 0}, /* TXA */

    {0x9A, 54, IMPLI, 2, 0}, /* TXS */

    {0x98, 55, IMPLI, 2, 0} /* TYA */
};

// FIXME: use stdint types
// FIXME: use g_ nomenclature for globals
unsigned short org; /* Origin of addresses */
char hex_output = 0; /* 1 if hex output is desired at beginning of line */
char cycle_counting = 0; /* 1 if we want cycle counting */
char nes_mode = 0; /* 1 if NES commenting and warnings are enabled */
FILE *f; /* Input file */
unsigned char buffer[0xffff]; /* Memory buffer */
unsigned short PC = 0; /* Program counter */
unsigned short max = 0xffff; /* Maximum number of bytes to disassemble */
char line[512];

/* This function emits a comment header with information about the file
   being disassembled */

void emit_header(char *filename, int fsize, unsigned short org) {
    fprintf(stdout, "; Source generated by DCC6502 version %s\n", VERSION_INFO);
    fprintf(stdout, "; For more info about DCC6502, see https://github.com/tcarmelveilleux/dcc6502\n");
    fprintf(stdout, "; FILENAME: %s, File Size: %d, ORG: $%04X\n", filename, fsize, org);
    if (hex_output) fprintf(stdout, ";     -> Hex output enabled\n");
    if (cycle_counting) fprintf(stdout, ";     -> Cycle counting enabled\n");
    if (nes_mode) fprintf(stdout, ";     -> NES mode enabled\n");
    fprintf(stdout, ";---------------------------------------------------------------------------\n");
}

/* This function appends cycle counting to the comment block */
void append_cycle(char *input, unsigned char entry, unsigned short arg, unsigned short cur_PC) {
    char tmpstr[256];
    int cycles = 0;

    cycles = opcode_table[entry].cycles;

    sprintf(tmpstr, " Cycles: %d ", cycles);
    if (opcode_table[entry].cross_page) {
        strcat(tmpstr, "*!* ");
    }
    strcat(input, tmpstr);
}

void add_nes_str(char *instr, char *instr2) {
    strcat(instr, " [NES] ");
    strcat(instr, instr2);
}

/* This function put NES-specific info in the comment block */
void append_nes(char *input, unsigned short arg) {
    switch(arg) {
        case 0x2000: add_nes_str(input, "PPU setup #1"); break;
        case 0x2001: add_nes_str(input, "PPU setup #2"); break;
        case 0x2002: add_nes_str(input, "PPU status"); break;
        case 0x2003: add_nes_str(input, "SPR-RAM address select"); break;
        case 0x2004: add_nes_str(input, "SPR-RAM data"); break;
        case 0x2005: add_nes_str(input, "PPU scroll"); break;
        case 0x2006: add_nes_str(input, "VRAM address select"); break;
        case 0x2007: add_nes_str(input, "VRAM data"); break;
        case 0x4000: add_nes_str(input, "Audio -> Square 1"); break;
        case 0x4001: add_nes_str(input, "Audio -> Square 1"); break;
        case 0x4002: add_nes_str(input, "Audio -> Square 1"); break;
        case 0x4003: add_nes_str(input, "Audio -> Square 1"); break;
        case 0x4004: add_nes_str(input, "Audio -> Square 2"); break;
        case 0x4005: add_nes_str(input, "Audio -> Square 2"); break;
        case 0x4006: add_nes_str(input, "Audio -> Square 2"); break;
        case 0x4007: add_nes_str(input, "Audio -> Square 2"); break;
        case 0x4008: add_nes_str(input, "Audio -> Triangle"); break;
        case 0x4009: add_nes_str(input, "Audio -> Triangle"); break;
        case 0x400a: add_nes_str(input, "Audio -> Triangle"); break;
        case 0x400b: add_nes_str(input, "Audio -> Triangle"); break;
        case 0x400c: add_nes_str(input, "Audio -> Noise control reg"); break;
        case 0x400e: add_nes_str(input, "Audio -> Noise Frequency reg #1"); break;
        case 0x400f: add_nes_str(input, "Audio -> Noise Frequency reg #2"); break;
        case 0x4010: add_nes_str(input, "Audio -> DPCM control"); break;
        case 0x4011: add_nes_str(input, "Audio -> DPCM D/A data"); break;
        case 0x4012: add_nes_str(input, "Audio -> DPCM address"); break;
        case 0x4013: add_nes_str(input, "Audio -> DPCM data length"); break;
        case 0x4014: add_nes_str(input, "Sprite DMA trigger"); break;
        case 0x4015: add_nes_str(input, "IRQ status / Sound enable"); break;
        case 0x4016: add_nes_str(input, "Joypad & I/O port for port #1"); break;
        case 0x4017: add_nes_str(input, "Joypad & I/O port for port #2"); break;
    }
}

// FIXME: Refactor code to reduce line duplication and make more readable 
/* This function disassembles the opcode at the PC and outputs it in *output */
void disassemble(char *output) {
    unsigned char tmp_byte1, tmp_byte2, opcode;
    char argument_signed;
    word tmp_word;

    char tmpstr[256], tmpstr2[256], tmpstr3[256];

    int i, entry, found = 0;

    opcode = buffer[PC];

    for (i = 0; i < NUMBER_OPCODES; i++) {
        if (opcode == opcode_table[i].number) {
            found = 1; /* Found the opcode */
            entry = i; /* Note the entry number in the table */
        }
    }

    if (!found) {
        if (hex_output) {
            sprintf(tmpstr, "$%04X> %02X:\t.byte $%02x\t\t; INVALID OPCODE !!!\n", org+PC, opcode, opcode);
            strncpy(output, tmpstr, 254);
        } else {
            sprintf(tmpstr, "$%04X\t.byte $%02x\t; INVALID OPCODE !!!\n", org+PC, opcode);
            strncpy(output, tmpstr, 254);
        }
    } else {
        switch (opcode_table[entry].addressing) {
            case IMMED:
                PC++;
                tmp_byte1 = buffer[PC]; /* Get immediate value */
                if (hex_output) {
                    sprintf(tmpstr, "$%04X> %02X %02X:\t%s #$%02x\t;", org+PC-1, opcode, tmp_byte1, name_table[opcode_table[entry].name], tmp_byte1);
                } else {
                    sprintf(tmpstr, "$%04X\t%s #$%02x\t;", org+PC-1, name_table[opcode_table[entry].name], tmp_byte1);
                }

                /* Add cycle count if necessary */
                if (cycle_counting) {
                    append_cycle(tmpstr, entry, org+PC-1, org+PC-1);
                }
                strncpy(output, tmpstr, 254);
                break;

            case ABSOL:
                PC++;
                tmp_word.B.l = buffer[PC]; /* Get low byte of address */
                PC++;
                tmp_word.B.h = buffer[PC]; /* Get high byte of address */

                if (hex_output)
                    sprintf(tmpstr, "$%04X> %02X %02X%02X:\t%s $%02X%02X\t;", org+PC-2, opcode, tmp_word.B.l, tmp_word.B.h, name_table[opcode_table[entry].name], tmp_word.B.h, tmp_word.B.l);
                else
                    sprintf(tmpstr, "$%04X\t%s $%02X%02X\t;", org+PC-2, name_table[opcode_table[entry].name], tmp_word.B.h, tmp_word.B.l);

                /* Add cycle count if necessary */
                if (cycle_counting) {
                    append_cycle(tmpstr, entry, tmp_word.W, org+PC-2);
                }

                /* Add NES port info if necessary */
                if (nes_mode) { 
                    append_nes(tmpstr, tmp_word.W);
                }
                strncpy(output, tmpstr, 254);
                break;

            case ZEROP:
                PC++;
                tmp_byte1 = buffer[PC]; /* Get low byte of address */

                if (hex_output) {
                    sprintf(tmpstr, "$%04X> %02X %02X:\t%s $%02X\t\t;", org+PC-1, opcode, tmp_byte1, name_table[opcode_table[entry].name], tmp_byte1);
                } else {
                    sprintf(tmpstr, "$%04X\t%s $%02X\t\t;", org+PC-1, name_table[opcode_table[entry].name], tmp_byte1);
                }
                
                /* Add cycle count if necessary */
                if (cycle_counting) {
                    append_cycle(tmpstr, entry, org+PC-1, org+PC-1);
                }

                strncpy(output, tmpstr, 254);
                break;

            case IMPLI:
                if (hex_output) {
                    sprintf(tmpstr, "$%04X> %02X:\t%s\t\t;", org+PC, opcode, name_table[opcode_table[entry].name]);
                } else {
                    sprintf(tmpstr, "$%04X\t%s\t\t;", org+PC, name_table[opcode_table[entry].name]);
                }

                /* Add cycle count if necessary */
                if (cycle_counting) {
                    append_cycle(tmpstr, entry, org+PC, org+PC);
                }

                strncpy(output, tmpstr, 254);
                break;

            case INDIA:
                PC++;
                tmp_word.B.l = buffer[PC]; /* Get low byte of address */
                PC++;
                tmp_word.B.h = buffer[PC]; /* Get high byte of address */

                if (hex_output) {
                    sprintf(tmpstr, "$%04X> %02X %02X%02X:\t%s ($%02X%02X)\t;", org+PC-2, opcode, tmp_word.B.l, tmp_word.B.h, name_table[opcode_table[entry].name], tmp_word.B.h, tmp_word.B.l);
                } else {
                    sprintf(tmpstr, "$%04X\t%s ($%02X%02X)\t;", org+PC-2, name_table[opcode_table[entry].name], tmp_word.B.h, tmp_word.B.l);
                }

                /* Add cycle count if necessary */
                if (cycle_counting) {
                    append_cycle(tmpstr, entry, tmp_word.W, org+PC-2);
                }

                strncpy(output, tmpstr, 254);
                break;

            case ABSIX:
                PC++;
                tmp_word.B.l = buffer[PC]; /* Get low byte of address */
                PC++;
                tmp_word.B.h = buffer[PC]; /* Get high byte of address */

                if (hex_output) {
                    sprintf(tmpstr, "$%04X> %02X %02X%02X:\t%s $%02X%02X,X\t;", org+PC-2, opcode, tmp_word.B.l, tmp_word.B.h, name_table[opcode_table[entry].name], tmp_word.B.h, tmp_word.B.l);
                } else {
                    sprintf(tmpstr, "$%04X\t%s $%02X%02X,X\t;", org+PC-2, name_table[opcode_table[entry].name], tmp_word.B.h, tmp_word.B.l);
                }

                /* Add cycle count if necessary */
                if (cycle_counting) {
                    append_cycle(tmpstr, entry, tmp_word.W, org+PC-2);
                }

                /* Add NES port info if necessary */
                if (nes_mode) { 
                    append_nes(tmpstr, tmp_word.W);
                }
                
                strncpy(output, tmpstr, 254);
                break;

            case ABSIY:
                PC++;
                tmp_word.B.l = buffer[PC]; /* Get low byte of address */
                PC++;
                tmp_word.B.h = buffer[PC]; /* Get high byte of address */

                if (hex_output) {
                    sprintf(tmpstr, "$%04X> %02X %02X%02X:\t%s $%02X%02X,Y\t;", org+PC-2, opcode, tmp_word.B.l, tmp_word.B.h, name_table[opcode_table[entry].name], tmp_word.B.h, tmp_word.B.l);
                } else {
                    sprintf(tmpstr, "$%04X\t%s $%02X%02X,Y\t;", org+PC-2, name_table[opcode_table[entry].name], tmp_word.B.h, tmp_word.B.l);
                }
                
                /* Add cycle count if necessary */
                if (cycle_counting) {
                    append_cycle(tmpstr, entry, tmp_word.W, org+PC-2);
                }

                /* Add NES port info if necessary */
                if (nes_mode) {
                    append_nes(tmpstr, tmp_word.W);
                }
                
                strncpy(output, tmpstr, 254);
                break;

            case ZEPIX:
                PC++;
                tmp_byte1 = buffer[PC]; /* Get low byte of address */

                if (hex_output) {
                    sprintf(tmpstr, "$%04X> %02X %02X:\t%s $%02X,X\t\t;", org+PC-1, opcode, tmp_byte1, name_table[opcode_table[entry].name], tmp_byte1);
                } else {
                    sprintf(tmpstr, "$%04X\t%s $%02X,X\t;", org+PC-1, name_table[opcode_table[entry].name], tmp_byte1);
                }

                /* Add cycle count if necessary */
                if (cycle_counting) {
                    append_cycle(tmpstr, entry, org+PC-1, org+PC-1);
                }

                strncpy(output, tmpstr, 254);
                break;

            case ZEPIY:
                PC++;
                tmp_byte1 = buffer[PC]; /* Get low byte of address */

                if (hex_output) {
                    sprintf(tmpstr, "$%04X> %02X %02X:\t%s $%02X,Y\t\t;", org+PC-1, opcode, tmp_byte1, name_table[opcode_table[entry].name], tmp_byte1);
                } else {
                    sprintf(tmpstr, "$%04X\t%s $%02X,Y\t;", org+PC-1, name_table[opcode_table[entry].name], tmp_byte1);
                }

                /* Add cycle count if necessary */
                if (cycle_counting) {
                    append_cycle(tmpstr, entry, org+PC-1, org+PC-1);
                }

                strncpy(output, tmpstr, 254);
                break;

            case INDIN:
                PC++;
                tmp_byte1 = buffer[PC]; /* Get low byte of address */

                if (hex_output) {
                    sprintf(tmpstr, "$%04X> %02X %02X:\t%s ($%02X,X)\t\t;", org+PC-1, opcode, tmp_byte1, name_table[opcode_table[entry].name], tmp_byte1);
                } else {
                    sprintf(tmpstr, "$%04X\t%s ($%02X,X)\t;", org+PC-1, name_table[opcode_table[entry].name], tmp_byte1);
                }

                /* Add cycle count if necessary */
                if (cycle_counting) {
                    append_cycle(tmpstr, entry, org+PC-1, org+PC-1);
                }

                strncpy(output, tmpstr, 254);
                break;

            case ININD:
                PC++;
                tmp_byte1 = buffer[PC]; /* Get low byte of address */

                if (hex_output) {
                    sprintf(tmpstr, "$%04X> %02X %02X:\t%s ($%02X),Y\t\t;", org+PC-1, opcode, tmp_byte1, name_table[opcode_table[entry].name], tmp_byte1);
                } else {
                    sprintf(tmpstr, "$%04X\t%s ($%02X),Y\t;", org+PC-1, name_table[opcode_table[entry].name], tmp_byte1);
                }

                /* Add cycle count if necessary */
                if (cycle_counting) {
                    append_cycle(tmpstr, entry, org+PC-1, org+PC-1);
                }

                strncpy(output, tmpstr, 254);
                break;

            case RELAT:
                PC++;
                tmp_byte1 = buffer[PC]; /* Get relative modifier */

                if (hex_output) {
                    sprintf(tmpstr, "$%04X> %02X %02X:\t%s $%04X\t\t;", org+PC-1, opcode, tmp_byte1, name_table[opcode_table[entry].name], (org+PC)+(signed char)(tmp_byte1)+1);
                } else {
                    sprintf(tmpstr, "$%04X\t%s $%04X\t;", org+PC-1, name_table[opcode_table[entry].name], (org+PC)+(signed char)(tmp_byte1)+1);
                }

                /* Add cycle count if necessary */
                if (cycle_counting) {
                    append_cycle(tmpstr, entry, org+PC, org+PC);
                }

                strncpy(output, tmpstr, 254);
                break;

            case ACCUM:
                if (hex_output) {
                    sprintf(tmpstr, "$%04X> %02X:\t%s A\t\t;", org+PC, opcode, name_table[opcode_table[entry].name]);
                } else {
                    sprintf(tmpstr, "$%04X\t%s A\t\t;", org+PC, name_table[opcode_table[entry].name]);
                }

                /* Add cycle count if necessary */
                if (cycle_counting) {
                    append_cycle(tmpstr, entry, org+PC, org+PC);
                }

                strncpy(output, tmpstr, 254);
                break;

            default:
                break;
        }
    }
}

void version(void) {
    fprintf(stderr, "DCC6502 %s (C)1998-2014 Tennessee Carmel-Veilleux <veilleux@tentech.ca>\n", VERSION_INFO);
    fprintf(stderr, "This software is licensed under the MIT license. See the LICENSE file.\n");
    fprintf(stderr, "See source on github: https://github.com/tcarmelveilleux/dcc6502.\n");
}

void usage_helper(char *str) {
    fprintf(stderr, "\t%s\n", str);
}

void usage(void) {
    usage_helper("-?      -> Show this help message");
    usage_helper("-oXXXX  -> Set the origin (ORG) [dfl: $8000]");
    usage_helper("-h      -> Get hex info about disassembly");
    usage_helper("-mXXXX  -> Only disassemble the first XXXX bytes");
    usage_helper("-n      -> Enable NES mode");
    usage_helper("-v      -> Get only version information");
    usage_helper("-c      -> Get cycle counting info");
    fprintf(stderr, "\n");
}

// FIXME: DE-KLUDGIFY THIS :D
unsigned short hex2int (char *str, unsigned short dfl) {
    char HEX_digits[16] = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'};
    int i, j;
    char c, k, shift;
    unsigned short tmp = 0;

    shift = 0;
    for (i = 5; i >= 2; i--) {
        if (!isxdigit(str[i])) {
            tmp = dfl;
            break;
        }
        c = toupper(str[i]);
        for (j = 0; j < 16; j++) {
            if (c == HEX_digits[j]) {
                k = j;
            }
        }
        tmp |= ((k & 0xf) << shift);
        shift += 4;
    }
    return tmp;
}

void set_org(char *str) {
    if (strlen(str) < 6) {
        fprintf(stderr, "WARNING -> %s is not a valid ORG switch, defaulting to $8000\n", str);
        org = 0x8000;
        return;
    }

    org = hex2int(str, 0x8000);
}

void set_max(char *str) {
    if (strlen(str) != 6) {
        max = 0xFFFF-org;
        fprintf(stderr, "WARNING -> %s is not a valid MAX switch, defaulting to $%04X\n", str, max);
        return;
    }

    max = hex2int(str, 0xFFFF);
}

int main(int argc, char *argv[]) {
    int i = 0;
    char tmpstring[512];
    char filename[512];

    cycle_counting = 0;
    hex_output = 0;
    org = 0x8000;

    if (argc < 2) {
        version();
        usage();
        exit(1);
    }

    if (argc > 2) {
        for (i = 1; i < argc - 1; i++) {
            if (argv[i][0] != '-') {
                version();
                usage();
                fprintf(stderr, "Unrecognized switch: %s\n", argv[i]);
                exit(1);
            }
            switch (argv[i][1]) {
                case '?':
                    version();
                    usage();
                    exit(0);
                    break;
                case 'n':
                    nes_mode = 1;
                    break;
                case 'c':
                    cycle_counting = 1;
                    break;
                case 'h':
                    hex_output = 1;
                    break;
                case 'v':
                    version();
                    exit(0);
                    break;
                case 'o':
                    set_org(argv[i]);
                    break;
                case 'm':
                    set_max(argv[i]);
                    break;
                default:
                    version();
                    usage();
                    fprintf(stderr, "Unrecognized switch: %s\n", argv[i]);
                    exit(1);
            }
        }
    } else {
        if (argv[1][0] != '-') {
            strncpy(filename, argv[1], 511);
        } else {
            switch (argv[1][1]) {
                case '?':
                    version();
                    usage();
                    exit(0);
                    break;
                case 'v':
                    version();
                    exit(0);
                    break;
                default:
                    version();
                    usage();
                    fprintf(stderr, "Unrecognized switch: %s\n", argv[1]);
                    exit(1);

            }
        }
    }

    strncpy(filename, argv[argc - 1], 511);

    f = fopen(filename, "rb");

    if (NULL == f) {
        version();
        fprintf(stderr, "File not found or invalid filename : %s\n", filename);
        exit(1);
    }

    i = 0;
    while(!feof(f) && ((i + org) < 65535)) {
        fread(&buffer[i], 1, 1, f);
        i++;
    }

    fclose(f);

    emit_header(filename, i, org);
    PC = 0;
    while(((PC + org) < 65535) && (PC <= max) && (PC < i)) {
        disassemble(tmpstring);
        fprintf(stdout, "%s\n", tmpstring);
        PC++;
    }

    return 0;
}

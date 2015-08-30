/* -*- mode: D -*-
 *
 * Copyright (c) 2007-2011, 2013, 2014 The University of Utah
 * All rights reserved.
 *
 * This file is part of `csmith', a random generator of C programs.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   * Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimer.
 *
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
module csmith;

alias uint8_t = ubyte;
alias uint16_t = ushort;
alias uint32_t = uint;
alias uint64_t = ulong;

alias int8_t = byte;
alias int16_t = short;
alias int32_t = int;
alias int64_t = long;

static uint32_t crc32_tab[256];
static uint32_t crc32_context = 0xFFFFFFFFUL;

void crc32_gentab()
{
	uint32_t crc;
	const uint32_t poly = 0xEDB88320UL;
	int i, j;
	
	for (i = 0; i < 256; i++) {
		crc = i;
		for (j = 8; j > 0; j--) {
			if (crc & 1) {
				crc = (crc >> 1) ^ poly;
			} else {
				crc >>= 1;
			}
		}
		crc32_tab[i] = crc;
	}
}

void crc32_byte (uint8_t b) {
	crc32_context = 
		((crc32_context >> 8) & 0x00FFFFFF) ^ 
		crc32_tab[(crc32_context ^ b) & 0xFF];
}


void crc32_8bytes (uint64_t val)
{
	crc32_byte ((val>>0) & 0xff);
	crc32_byte ((val>>8) & 0xff);
	crc32_byte ((val>>16) & 0xff);
	crc32_byte ((val>>24) & 0xff);
	crc32_byte ((val>>32) & 0xff);
	crc32_byte ((val>>40) & 0xff);
	crc32_byte ((val>>48) & 0xff);
	crc32_byte ((val>>56) & 0xff);
}

extern(C) int putchar(int ch);

void transparent_crc (uint64_t val,  const(char)[] vname, int flag)
{
	crc32_8bytes(val);
	if (flag) {
  		ulong x = crc32_context ^ 0xFFFFFFFFUL;
		my_puts ("...checksum after hashing ");
		my_puts(vname);
		putchar(' ');
	    for (int i=0; i<16; i++) {
	      put_hex (x & 0xf);
	      x >>= 4;
	    }
	    putchar ('\n');
	}
}


void transparent_crc_bytes (char *ptr, int nbytes, const(char)[] vname, int flag)
{
    int i;
    for (i=0; i<nbytes; i++) {
        crc32_byte(ptr[i]);
    }
	if (flag) {
		ulong x = crc32_context ^ 0xFFFFFFFFUL;
		my_puts ("...checksum after hashing ");
		my_puts(vname);
		putchar(' ');
	    for (i=0; i<16; i++) {
	      put_hex (x & 0xf);
	      x >>= 4;
	    }
	    putchar ('\n');
	}
}

void my_puts (const(char)[] p)
{
  foreach (char c; p)
    putchar (c);
}

void put_hex (int x)
{
  switch (x) {
  case 0x0: putchar ('0'); break;
  case 0x1: putchar ('1'); break;
  case 0x2: putchar ('2'); break;
  case 0x3: putchar ('3'); break;
  case 0x4: putchar ('4'); break;
  case 0x5: putchar ('5'); break;
  case 0x6: putchar ('6'); break;
  case 0x7: putchar ('7'); break;
  case 0x8: putchar ('8'); break;
  case 0x9: putchar ('9'); break;
  case 0xa: putchar ('a'); break;
  case 0xb: putchar ('b'); break;
  case 0xc: putchar ('c'); break;
  case 0xd: putchar ('d'); break;
  case 0xe: putchar ('e'); break;
  case 0xf: putchar ('f'); break;
  default: break;
  }
}

void platform_main_begin(){}

void platform_main_end (uint64_t x, int flag)
{
  if (!flag) {
    int i;
    my_puts ("checksum = ");
    for (i=0; i<16; i++) {
      put_hex (x & 0xf);
      x >>= 4;
    }
    putchar ('\n');
  }
}


auto safe_unary_minus_func_t_s(T,L)(auto ref L _si){
  T si = cast(T)_si;
  return si == T.min ? si : -si;
}

auto safe_add_func_s_s(T, L, R)(auto ref L _si1, auto ref R _si2){
  T zero = 0;
  T si1 = cast(T)_si1; T si2 = cast(T)_si2;
  return  si1>zero && si2>zero && si1 > (T.max - si2)
    || si1 < zero && si2 < zero && si1 < (T.min - si2) ?
   si1 :  si1 + si2;
}

auto safe_sub_func_s_s(T,L,R)(auto ref L _si1, auto ref R _si2){
    T zero = 0;
    T si1 = cast(T)_si1; T si2 = cast(T)_si2;
    ((si1^si2) & (((si1 ^ ((si1^si2) & ((cast(T)1) << (T.sizeof*8-1))))-si2)^si2)) < zero
    ? si1: (si1 - si2);
}
/*
auto safe_mul_func_s_s(T,L,R)(auto ref L _si1, auto ref R _si2){
  T zero = 0;
  T si1 = cast(T)_si1; T si2 = cast(T)_si2;
  return (((si1 > zero && (si2 > zero && (si1 > (T.max/ si2))) ||
  ((si1 > zero && (si2 <= zero && (si2 < (T.min / si1))) ||
  ((si1 <= zero && (si2 > zero && (si1 < (T.min / si2))) ||
  ((si1 <= zero && (si2 <= zero && (si1 != zero && (si2 < (T.max/ si1))))
  ? si1
  : si1 * si2;
}

auto safe_mod_func_s_s(T,L,R)(auto ref L _si1, auto ref R _si2)
  ({ T si1 = (_si1); T si2 = (_si2) ;
  ((si2 == zero || ((si1 == T.min) && (si2 == ((T)-1))))
  ? si1
  : (si1 % si2);})

auto safe_div_func_s_s(T,L,R)(auto ref L _si1, auto ref R _si2)
  ({ T si1 = (_si1); T si2 = (_si2) ;
  ((si2 == zero || ((si1 == T.min) && (si2 == ((T)-1))))
  ? si1
  : (si1 / si2);})

auto safe_lshift_func_s_s(T,L,R)(_left,_right)
  ({ T left = _left; int right = _right ;
   ((((T)left) < zero
  || ((intright) < zero
  || ((intright) >= T.sizeof*8)
  || (((T)left) > (T.max>> (intright))))
  ? ((T)left)
  : (((T)left) << (intright));})

auto safe_lshift_func_s_u(T,L,R)(_left,_right)
  ({ T left = _left; unsigned int right = _right ;
   ((((T)left) < zero
  || (((unsigned int)right) >= T.sizeof*8)
  || (((T)left) > (T.max>> ((unsigned int)right))))
  ? ((T)left)
  : (((T)left) << ((unsigned int)right));})

auto safe_rshift_func_s_s(T,L,R)(_left,_right)
  ({ T left = _left; int right = _right ;
        ((((T)left) < zero
       || ((intright) < zero
       || ((intright) >= T.sizeof*8))
      ? ((T)left)
      : (((T)left) >> (intright));})

auto safe_rshift_func_s_u(T,L,R)(_left,_right)
  ({ T left = _left; unsigned int right = _right ;
   ((((T)left) < zero
       || (((unsigned int)right) >= T.sizeof*8))
      ? ((T)left)
      : (((T)left) >> ((unsigned int)right));})

*/
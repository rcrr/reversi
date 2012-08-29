/*
 *  BitWorksTest.java
 *
 *  Copyright (c) 2012 Roberto Corradini. All rights reserved.
 *
 *  This file is part of the reversi program
 *  http://github.com/rcrr/reversi
 *
 *  This program is free software; you can redistribute it and/or modify it
 *  under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3, or (at your option) any
 *  later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA
 *  or visit the site <http://www.gnu.org/licenses/>.
 */

package rcrr.reversi.board;

import java.util.Map;
import java.util.HashMap;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.instanceOf;

/**
 * Test Suite for {@code BitWorks} class.
 */
public class BitWorksTest {

    @Test
    public final void testDummy() {

        assertTrue(true);

    }

    @Test
    public final void testBitscanLS1B() {

        /** Never send 0L as parameter. */
        assertThat("BitWorks.bitscanLS1B(0L) is 63.",
                   BitWorks.bitscanLS1B(0L),
                   is(63));

        assertThat("BitWorks.bitscanLS1B(1L) is 0.",
                   BitWorks.bitscanLS1B(1L),
                   is(0));
        assertThat("BitWorks.bitscanLS1B(0x8000000000000000L) is 63.",
                   BitWorks.bitscanLS1B(0x8000000000000000L),
                   is(63));
        assertThat("BitWorks.bitscanLS1B(0x8000000000000001L) is 0.",
                   BitWorks.bitscanLS1B(0x8000000000000001L),
                   is(0));
        assertThat("BitWorks.bitscanLS1B(0x0000000180000000L) is 31.",
                   BitWorks.bitscanLS1B(0x0000000180000000L),
                   is(31));
    }

    @Test
    public final void testBitscanLS1B0() {

        assertThat("BitWorks.bitscanLS1B0(0L) is 64.",
                   BitWorks.bitscanLS1B0(0L),
                   is(64));
        assertThat("BitWorks.bitscanLS1B0(1L) is 0.",
                   BitWorks.bitscanLS1B0(1L),
                   is(0));
        assertThat("BitWorks.bitscanLS1B0(0x8000000000000000L) is 63.",
                   BitWorks.bitscanLS1B0(0x8000000000000000L),
                   is(63));
        assertThat("BitWorks.bitscanLS1B0(0x8000000000000001L) is 0.",
                   BitWorks.bitscanLS1B0(0x8000000000000001L),
                   is(0));
        assertThat("BitWorks.bitscanLS1B0(0x0000000180000000L) is 31.",
                   BitWorks.bitscanLS1B0(0x0000000180000000L),
                   is(31));
    }

    @Test
    public final void testBitscanMS1B() {

        /** Never send 0L as parameter. */
        assertThat("BitWorks.bitscanMS1B(0L) is 0.",
                   BitWorks.bitscanMS1B(0L),
                   is(0));

        assertThat("BitWorks.bitscanMS1B(1L) is 0.",
                   BitWorks.bitscanMS1B(1L),
                   is(0));
        assertThat("BitWorks.bitscanMS1B(0x8000000000000000L) is 63.",
                   BitWorks.bitscanMS1B(0x8000000000000000L),
                   is(63));
        assertThat("BitWorks.bitscanMS1B(0x8000000000000001L) is 63.",
                   BitWorks.bitscanMS1B(0x8000000000000001L),
                   is(63));
        assertThat("BitWorks.bitscanMS1B(0x0000000180000000L) is 32.",
                   BitWorks.bitscanMS1B(0x0000000180000000L),
                   is(32));
    }

    @Test
    public final void testBitscanMS1B0() {

        /** Never send 0L as parameter. */
        assertThat("BitWorks.bitscanMS1B0(0L) is 0.",
                   BitWorks.bitscanMS1B0(0L),
                   is(0));

        assertThat("BitWorks.bitscanMS1B0(1L) is 0.",
                   BitWorks.bitscanMS1B0(1L),
                   is(0));
        assertThat("BitWorks.bitscanMS1B0(0x8000000000000000L) is 63.",
                   BitWorks.bitscanMS1B0(0x8000000000000000L),
                   is(63));
        assertThat("BitWorks.bitscanMS1B0(0x8000000000000001L) is 63.",
                   BitWorks.bitscanMS1B0(0x8000000000000001L),
                   is(63));
        assertThat("BitWorks.bitscanMS1B0(0x0000000180000000L) is 32.",
                   BitWorks.bitscanMS1B0(0x0000000180000000L),
                   is(32));
        assertThat("BitWorks.bitscanMS1B0(0x0000000280000000L) is 33.",
                   BitWorks.bitscanMS1B0(0x0000000280000000L),
                   is(33));
    }

    @Test
    public final void testHighestBitSet() {

        /** Never send 0 as parameter. */
        assertThat("BitWorks.highestBitSet(0) is 0.",
                   BitWorks.highestBitSet(0),
                   is(0));

        assertThat("BitWorks.highestBitSet(1) is 1.",
                   BitWorks.highestBitSet(1),
                   is(1));
        assertThat("BitWorks.highestBitSet(0x80000000) is 0x80000000.",
                   BitWorks.highestBitSet(0x80000000),
                   is(0x80000000));
        assertThat("BitWorks.highestBitSet(0x80000001) is 31.",
                   BitWorks.highestBitSet(0x80000000),
                   is(0x80000000));
        assertThat("BitWorks.highestBitSet(0x00018000) is 0x00010000.",
                   BitWorks.highestBitSet(0x00018000),
                   is(0x00010000));
        assertThat("BitWorks.highestBitSet(0x0F0A0300) is 0x08000000.",
                   BitWorks.highestBitSet(0x0F0A0300),
                   is(0x08000000));
    }

    @Test
    public final void testLongToString() {

        assertThat("BitWorks.longToString(0L) is 00000000.00000000.00000000.00000000.00000000.00000000.00000000.00000000.",
                   BitWorks.longToString(0L),
                   is("00000000.00000000.00000000.00000000.00000000.00000000.00000000.00000000"));

        assertThat("BitWorks.longToString(1L) is 00000000.00000000.00000000.00000000.00000000.00000000.00000000.00000001.",
                   BitWorks.longToString(1L),
                   is("00000000.00000000.00000000.00000000.00000000.00000000.00000000.00000001"));

        assertThat("BitWorks.longToString(2L) is 00000000.00000000.00000000.00000000.00000000.00000000.00000000.00000010.",
                   BitWorks.longToString(2L),
                   is("00000000.00000000.00000000.00000000.00000000.00000000.00000000.00000010"));

        assertThat("BitWorks.longToString(-1L) is 11111111.11111111.11111111.11111111.11111111.11111111.11111111.11111111.",
                   BitWorks.longToString(-1L),
                   is("11111111.11111111.11111111.11111111.11111111.11111111.11111111.11111111"));

        assertThat("BitWorks.longToString(1L) is 10000000.00000000.00000000.00000000.00000000.00000000.00000000.00000000.",
                   BitWorks.longToString(-9223372036854775808L),
                   is("10000000.00000000.00000000.00000000.00000000.00000000.00000000.00000000"));

        assertThat("BitWorks.longToString(0x8000000000000000L) is 10000000.00000000.00000000.00000000.00000000.00000000.00000000.00000000.",
                   BitWorks.longToString(0x8000000000000000L),
                   is("10000000.00000000.00000000.00000000.00000000.00000000.00000000.00000000"));

    }

    @Test
    public final void testLowestBitSet_long() {

        assertThat("BitWorks.lowestBitSet(0L) is 0L.",
                   BitWorks.lowestBitSet(0L),
                   is(0L));
        assertThat("BitWorks.lowestBitSet(1L) is 1L.",
                   BitWorks.lowestBitSet(1L),
                   is(1L));
        assertThat("BitWorks.lowestBitSet(0x8000000000000000L) is 0x8000000000000000L.",
                   BitWorks.lowestBitSet(0x8000000000000000L),
                   is(0x8000000000000000L));
        assertThat("BitWorks.lowestBitSet(0xFFFFFFFFFFFFFFFFL) is 0x0000000000000001L.",
                   BitWorks.lowestBitSet(0xFFFFFFFFFFFFFFFFL),
                   is(0x0000000000000001L));
        assertThat("BitWorks.lowestBitSet(0x8000000000000001L) is 0x0000000000000001L.",
                   BitWorks.lowestBitSet(0x8000000000000001L),
                   is(0x0000000000000001L));
        assertThat("BitWorks.lowestBitSet(0xFF00000000000000L) is 0x0100000000000000L.",
                   BitWorks.lowestBitSet(0x0100000000000000L),
                   is(0x0100000000000000L));
        assertThat("BitWorks.lowestBitSet(0x0000000180000000L) is 0x0000000080000000L.",
                   BitWorks.lowestBitSet(0x0000000180000000L),
                   is(0x0000000080000000L));
        assertThat("BitWorks.lowestBitSet(0x0F0A0300CD057900L) is 0x0000000000000100L.",
                   BitWorks.lowestBitSet(0x0F0A0300CD057900L),
                   is(0x0000000000000100L));
    }

    @Test
    public final void testLowestBitSet_int() {

        assertThat("BitWorks.lowestBitSet(0) is 0.",
                   BitWorks.lowestBitSet(0),
                   is(0));
        assertThat("BitWorks.lowestBitSet(1) is 1.",
                   BitWorks.lowestBitSet(1),
                   is(1));
        assertThat("BitWorks.lowestBitSet(0x80000000) is 0x80000000.",
                   BitWorks.lowestBitSet(0x80000000),
                   is(0x80000000));
        assertThat("BitWorks.lowestBitSet(0xFFFFFFFF) is 0x00000001.",
                   BitWorks.lowestBitSet(0xFFFFFFFF),
                   is(0x0000001));
        assertThat("BitWorks.lowestBitSet(0x80000001) is 0x00000001.",
                   BitWorks.lowestBitSet(0x80000001),
                   is(0x00000001));
        assertThat("BitWorks.lowestBitSet(0xFF000000) is 0x0100000.",
                   BitWorks.lowestBitSet(0x01000000),
                   is(0x01000000));
        assertThat("BitWorks.lowestBitSet(0x00018000) is 0x00008000.",
                   BitWorks.lowestBitSet(0x00018000),
                   is(0x00008000));
        assertThat("BitWorks.lowestBitSet(0xCD057900) is 0x00000100.",
                   BitWorks.lowestBitSet(0xCD057900),
                   is(0x00000100));
    }

    @Test
    public final void testFillInBetween() {
        assertThat("BitWorks.fillInBetween(0x81) is 0x7E [10000001 -> 01111110].",
                   BitWorks.fillInBetween(0x81),
                   is(0x7E));
        assertThat("BitWorks.fillInBetween(0x28) is 0x10 [00101000 -> 00010000].",
                   BitWorks.fillInBetween(0x28),
                   is(0x10));
    }

    @Test
    public final void testSignedLeftShift() {

        /**
         * 0xFFFFFFFFFFFFFFFFL is 11111111.11111111.11111111.11111111.11111111.11111111.11111111.11111111.
         * Shifting by 0 gives:   11111111.11111111.11111111.11111111.11111111.11111111.11111111.11111111.
         * that is 0xFFFFFFFFFFFFFFFFL.
         */
        assertThat("BitWorks.signedLeftShift(0xFFFFFFFFFFFFFFFFL, 0) is 0xFFFFFFFFFFFFFFFFL",
                   BitWorks.signedLeftShift(0xFFFFFFFFFFFFFFFFL, 0),
                   is(0xFFFFFFFFFFFFFFFFL));

        /**
         * 0xFFFFFFFFFFFFFFFFL is 11111111.11111111.11111111.11111111.11111111.11111111.11111111.11111111.
         * Shifting by 1 gives:   11111111.11111111.11111111.11111111.11111111.11111111.11111111.11111110.
         * that is 0xFFFFFFFFFFFFFFFFL.
         */
        assertThat("BitWorks.signedLeftShift(0xFFFFFFFFFFFFFFFFL, 1) is 0xFFFFFFFFFFFFFFFEL",
                   BitWorks.signedLeftShift(0xFFFFFFFFFFFFFFFFL, 1),
                   is(0xFFFFFFFFFFFFFFFEL));

        /**
         * 0xFFFFFFFFFFFFFFFFL is 11111111.11111111.11111111.11111111.11111111.11111111.11111111.11111111.
         * Shifting by 63 gives:  10000000.00000000.00000000.00000000.00000000.00000000.00000000.00000000.
         * that is 0x8000000000000000L.
         */
        assertThat("BitWorks.signedLeftShift(0xFFFFFFFFFFFFFFFFL, 63) is 0x8000000000000000L",
                   BitWorks.signedLeftShift(0xFFFFFFFFFFFFFFFFL, 63),
                   is(0x8000000000000000L));

        /**
         * 0xFFFFFFFFFFFFFFFFL is 11111111.11111111.11111111.11111111.11111111.11111111.11111111.11111111.
         * Shifting by 64 gives:  11111111.11111111.11111111.11111111.11111111.11111111.11111111.11111111.
         * that is 0xFFFFFFFFFFFFFFFFL.
         * Because shift is masked mod 64 on longs by the java shift operation.
         */
        assertThat("BitWorks.signedLeftShift(0xFFFFFFFFFFFFFFFFL, 64) is 0xFFFFFFFFFFFFFFFFL",
                   BitWorks.signedLeftShift(0xFFFFFFFFFFFFFFFFL, 64),
                   is(0xFFFFFFFFFFFFFFFFL));

        /**
         * 0xFFFFFFFFFFFFFFFFL is 11111111.11111111.11111111.11111111.11111111.11111111.11111111.11111111.
         * Shifting by -1 gives:01111111.11111111.11111111.11111111.11111111.11111111.11111111.11111111.
         * that is 0x7FFFFFFFFFFFFFFFL.
         */
        assertThat("BitWorks.signedLeftShift(0xFFFFFFFFFFFFFFFFL, -1) is 0x7FFFFFFFFFFFFFFFL",
                   BitWorks.signedLeftShift(0xFFFFFFFFFFFFFFFFL, -1),
                   is(0x7FFFFFFFFFFFFFFFL));
    }


}

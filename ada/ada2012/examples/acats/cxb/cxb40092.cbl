      * CXB40092.CBL
      * 
      *                         Grant of Unlimited Rights
      *
      * Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
      * F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained 
      * unlimited rights in the software and documentation contained herein.
      * Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making 
      * this public release, the Government intends to confer upon all 
      * recipients unlimited rights  equal to those held by the Government.  
      * These rights include rights to use, duplicate, release or disclose the 
      * released technical data and computer software in whole or in part, in 
      * any manner and for any purpose whatsoever, and to have or permit others 
      * to do so.
      *
      *                                DISCLAIMER
      *
      * ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
      * DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED 
      * WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
      * SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE 
      * OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
      * PARTICULAR PURPOSE OF SAID MATERIAL.
      *
      * PROCEDURE NAME:  CXB40092   ("Copy_and_Double")
      *
      * PROCEDURE DESCRIPTION:
      *      This COBOL procedure accepts a four byte binary integer, copies
      *      it into a four byte binary integer Out parameter, then doubles 
      *      the In parameter value and sets the doubled value into a second
      *      four byte binary integer Out parameter.
      *
      * INPUTS:
      *      A four byte binary integer.
      *
      * PROCESSING:
      *      The four byte binary integer In parameter is copied into one of
      *      the four byte binary integer Out parameters.  The In parameter
      *      value is then doubled and placed in the other four byte binary
      *      integer Out parameter.
      *
      * OUTPUTS:
      *      A pair of four byte binary integers, one containing a copy of 
      *      the In parameter, the other containing the doubled value of the
      *      In parameter.
      *
      * CHANGE HISTORY:
      *    23 Feb 1996   SAIC   Initial release for ACVC 2.1.
      *    10 Jun 1996   SAIC   Incorporated reviewer comments for ACVC 2.1.
      *    26 Jun 1998   EDS    Substituted COBOL code provided by RBK Dewar
      *                         for original incorrect COBOL code
      * 

       IDENTIFICATION DIVISION.
       PROGRAM-ID. CXB40092.

      * This program accepts a 4 byte binary integer and copies it
      * to another 4 byte binary integer, doubles it, and passes
      * the doubled value back as well, also in a 4 byte binary
      * integer format.
      *
      *
      *               GENERAL COMMENTS ON COBOL
      * COBOL is column based. Division names, section names, Data
      * Division 01 level storage areas and Procedure Division
      * paragraph names must start in column 8.
      *
      * Data Division 02 - 49 level storage areas start in column 12,
      * as do Procedure Division sentences. Indentation is permitted
      * between columns 12 and 70.
      *
      * It is best not to use columns 1 - 6 and 71 - 80.
      * An asterisk (*) in column 7 indicates a comment entry.
      * Blank lines are acceptable for readability, just as in Ada.
      * Use CAPITAL letters. Newer versions of the language support
      * mixed case. Older versions and certain platforms may not.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * All local variables are listed in the Working-Storage Section.
      * Recommend using only 01 levels because COBOL will boundary
      * align binary values, as needed and due to the simplicity of
      * these programs.
      *
      * 88 levels can only be tested. They cannot be moved or
      * otherwise manipulated.

       01  PROGRAM-NAME          PIC X(8)   VALUE 'CXB40092'.

       LINKAGE SECTION.

      * All passed variables are listed in the Linkage Section. The
      * Linkage Section is only present if there are parameters
      * passed into or out of the COBOL program. Recommend using only
      * 01 levels in the Linkage Section, unless record passing is
      * being tested. All 01 levels in the Linkage Section must be
      * named in the USING clause of the Procedure Division statement.

      * These examples support parameter passing by reference.

       01  IN-INTEGER           PIC S9999  USAGE IS BINARY.
       01  OUT-INTEGER          PIC S9999  USAGE IS BINARY.
       01  OUT-DOUBLE           PIC S9999  USAGE IS BINARY.

      * The USAGE IS BINARY clause specifies that a radix of 2
      * is used to represent a numeric item in the storage of a
      * computer. Each implementor specifies the precise effect
      * of the USAGE IS BINARY clause upon the alignment and
      * representation of the data item in the storage of the
      * computer, including the representation of any algebraic
      * sign. Sufficient computer storage must be allocated by
      * the implementor to contain the maximum range of values
      * implied by the associated decimal PICTURE character
      * string. (ANSI X3.23-1985, American National Standard
      * for Information Systems - Programming Language - COBOL,
      * page VI-47).

       PROCEDURE DIVISION USING IN-INTEGER
                               OUT-INTEGER
                               OUT-DOUBLE.
       MAIN.

           MOVE IN-INTEGER TO OUT-INTEGER.
           ADD IN-INTEGER OUT-INTEGER GIVING OUT-DOUBLE.
           EXIT PROGRAM.

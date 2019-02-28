       identification division.
       program-id. Program01_Edit.
       author. Shreeji Patel, Ryan Beckett, Mathew Kosterzewa.
       Date-Written. 09-Apr-2018.
      *Purpose     : This file is created for the purpose of the group
      *              project in MAFD-4202.
      *Description : This program sorts the valid and invalid data
      *              according to the given business rules and prints
      *              them in different files. It also creates the 
      *              errors report indicating the messages for wrong
      *              fields.

       environment division.

       input-output section.
       file-control.
      * both input and output files are configured
           select input-file  assign       to
                                       "../../../data/project1.dat"
               organization is line sequential.
                    
           select valid-file assign        to
                                   "../../../data/project1Valid.out"
               organization is line sequential.

           select invalid-file assign      to
                                   "../../../data/project1Invalid.out"
               organization is line sequential.

           select errors-file assign       to
                                   "../../../data/project1Errors.out"
               organization is line sequential.

       data division.
       file section.
      *input file record
       fd input-file 
           data record is input-line
               record contains 37 characters.

      *input line
       01 input-line.
           05 il-trans-code                pic x.
               88 il-valid-code        value 'S', 'R', 'L'.
           05 il-trans-amount              pic 9(5)v99.
           05 il-pay-type                  pic xx.
               88 il-valid-type        value 'CA', 'CR', 'DB'.
           05 il-store-num                 pic xx.
               88 il-valid-str-num     value '01', '02', '03', '07'. 
           05 il-invoice-num.
               10 il-invoice-alpha-1       pic x.
                   88 il-valid-alpha-1 value 'A' thru 'Z'.
               10 il-invoice-alpha-2       pic x.
                   88 il-valid-alpha-2 value 'A' thru 'Z'.
               10 il-invoice-sep           pic x.
               10 il-valid-inv-num         pic 9(6).
           05 il-sku-code                  pic x(15). 

      *valid file record
       fd valid-file 
           data record is valid-line
               record contains 36 characters. 
       01 valid-line                       pic x(36)
           value spaces.

      *invalid file record 
       fd invalid-file 
           data record is invalid-line
               record contains 36 characters. 
       01 invalid-line                     pic x(36)
           value spaces.

      *errors file record
       fd errors-file
           data record is errors-line
               record contains 200 characters. 
       01 errors-line                      pic x(200)
           value spaces.
       
       working-storage section.

      *error detail line
       01 ws-errors-details.
           05 filler                       pic x
               value spaces.
           05 ws-actual-record             pic x(36)
               value spaces.
           05 filler                       pic xx
               value spaces.
           05 ws-in-trans-code             pic x(18)
               value spaces.
           05 filler                       pic xx
               value spaces.
           05 ws-in-trans-amt              pic x(20)
               value spaces.
           05 filler                       pic xx
               value spaces.
           05 ws-in-pay-type               pic x(16)
               value spaces.
           05 filler                       pic xx
               value spaces. 
           05 ws-in-str-num                pic x(19)
               value spaces.
           05 filler                       pic xx
               value spaces. 
           05 ws-in-inv-num                pic x(19)
               value spaces.
           05 filler                       pic xx
               value spaces.
           05 ws-in-sku-code               pic X(14)
               value spaces.

      *report header
       01 ws-error-header.
           05 filler                       pic x(28)   
               value spaces.
           05 filler                       pic x(25)
               value "ERRORS REPORT".
           05 ws-date                      pic 99/99/99
               value spaces.
           05 filler                       pic x(15)
               value spaces.
           05 ws-time                      pic 99b99b99b99
               value spaces.

      *report column headings
       01 ws-error-headings-1.
           05 filler                       pic x(10)
               value spaces.
           05 filler                       pic x(31)
               value "ACTUAL RECORD".
           05 filler                       pic x(21)
               value "TRANSACTION".
           05 filler                       pic x(21)
               value "TRANSACTION".
           05 filler                       pic x(20)
               value "PAYMENT".
           05 filler                       pic x(21)
               value "STORE".
           05 filler                       pic x(18)
               value "INVOICE".
           05 filler                       pic x(15)
               value "SKU-CODE".

       01 ws-error-headings-2.
           05 filler                       pic x(44)
               value spaces.
           05 filler                       pic x(20)
               value "CODE".
           05 filler                       pic x(20)
               value "AMOUNT".
           05 filler                       pic x(19)
               value "TYPE".
           05 filler                       pic x(21)
               value "NUMBER".
           05 filler                       pic x(30)
               value "NUMBER".

      *record for the footer line(number of total, valid, invalid)
       01 ws-page-footer.
           05 filler                       pic x
               value spaces.
           05 filler                       pic x(16)
               value "Total Records = ".
           05 ws-pf-total-records          pic zz9
               value 0.
           05 filler                       pic x(5)
               value spaces.
           05 filler                       pic x(16)
               value "Valid Records = ".
           05 ws-pf-total-valid            pic zz9
               value 0.
           05 filler                       pic x(5)
               value spaces.
           05 filler                       pic x(18)
               value "Invalid Records = ".
           05 ws-pf-total-invalid          pic zz9
               value 0.

      *underlines
       01 ws-error-underlines.
           05 filler                       pic x(10)
               value spaces.
           05 filler                       pic x(31)
               value "-------------".
           05 filler                       pic x(21)
               value "-----------".
           05 filler                       pic x(21)
               value "-----------".
           05 filler                       pic x(20)
               value "-------".
           05 filler                       pic x(21)
               value "------".
           05 filler                       pic x(18)
               value "-------".
           05 filler                       pic x(8)
               value "--------".
           
       01 ws-eof                           pic x
           value 'n'.

      *variables for the total valid, invalid records
      *and number of errors 
       01 ws-total-records                 pic 999
           value 0.
       01 ws-cnt-errors                    pic 9
	       value 0.
       01 ws-total-valid-records           pic 999
           value 0.
       01 ws-total-invalid-records         pic 999
           value 0.
       
      *constants for the error messages
       77 ws-in-trans-code-msg             pic x(18)
           value "INVALID TRANS-CODE".
       77 ws-in-trans-amt-msg              pic x(20)
           value "INVALID TRANS-AMOUNT".
       77 ws-in-pay-type-msg               pic x(16)
           value "INVALID PAY-TYPE".
       77 ws-in-inv-num-msg                pic x(19)
           value "INVALID INVOICE-NUM".
       77 ws-in-str-num-msg                pic x(17)
           value "INVALID STORE-NUM".
       77 ws-in-sku-code-msg               pic x(14)
           value "EMPTY SKU-CODE".

       procedure division.
           open input input-file
                output valid-file invalid-file errors-file.

      *header and column headings
           accept ws-date              from date.
           accept ws-time              from time.
           write errors-line           from ws-error-header
               after advancing 1 line.
           write errors-line           from spaces.
           write errors-line           from spaces.
           write errors-line           from ws-error-headings-1.
           write errors-line           from ws-error-headings-2.
           write errors-line           from ws-error-underlines.
           write errors-line           from spaces.

           read input-file
               at end move "y"             to ws-eof.

          perform until ws-eof equals "y"
               add 1                       to ws-total-records
			   move 0                      to ws-cnt-errors
			   move input-line             to ws-actual-record
			   move spaces                 to ws-in-trans-code
               move spaces                 to ws-in-trans-amt
               move spaces                 to ws-in-pay-type
               move spaces                 to ws-in-str-num
               move spaces                 to ws-in-inv-num
               move spaces                 to ws-in-sku-code

      *check for specific errors using 88's defined above and
      *move the error message to the errors file
			   if (not il-valid-code)
				   add 1                   to ws-cnt-errors
				   move
                     ws-in-trans-code-msg  to ws-in-trans-code
               end-if
			   if (not il-trans-amount is numeric)
				   add 1                   to ws-cnt-errors
				   move
                     ws-in-trans-amt-msg   to ws-in-trans-amt
               end-if
			   if (not il-valid-type)
				   add 1                   to ws-cnt-errors
				   move ws-in-pay-type-msg to ws-in-pay-type
               end-if
			   if (not il-valid-str-num)
				   add 1                   to ws-cnt-errors
				   move ws-in-str-num-msg  to ws-in-str-num
               end-if
			   if (not il-valid-alpha-1)
				   add 1                   to ws-cnt-errors
				   move ws-in-inv-num-msg  to ws-in-inv-num
               end-if
			   if (not il-valid-alpha-2)
				   add 1                   to ws-cnt-errors
				   move ws-in-inv-num-msg  to ws-in-inv-num
               end-if
			   if (not il-valid-inv-num is numeric)
				   add 1                   to ws-cnt-errors
				   move ws-in-inv-num-msg  to ws-in-inv-num
               end-if
			   if (not il-sku-code not equal spaces)
				   add 1                   to ws-cnt-errors
				   move ws-in-sku-code-msg to ws-in-sku-code	
               end-if
			   if (ws-cnt-errors > 0)
                   add 1 to ws-total-invalid-records
				   write errors-line   from ws-errors-details
                   write invalid-line  from input-line
			   else
                   add 1 to ws-total-valid-records
				   write valid-line    from input-line
               end-if

               read input-file
                   at end move "y"         to ws-eof

           end-perform.

      *move the calulated values to the record and write the line
           move ws-total-records           to ws-pf-total-records.
           move ws-total-invalid-records   to ws-pf-total-invalid.
           move ws-total-valid-records     to ws-pf-total-valid.

           write errors-line           from ws-page-footer
               after advancing 2 line.

      *close all the files
           close input-file, valid-file, invalid-file, errors-file.

           goback.
       
       end program Program01_Edit.

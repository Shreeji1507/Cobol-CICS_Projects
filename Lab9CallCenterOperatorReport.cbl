       identification division.
       program-id. Lab9CallCenterOperatorReport.
       author. Gregory Oakes, Shreeji Patel.
       Date-Written. 21-Apr-2018.
      *Purpose : This file is created for the lab 9 in MAFD-4202.
      *Description : This program uses arrays to store and use the
      *              monthly calls and creates a report based on the 
      *              given data file	

       environment division.
       input-output section.
       file-control.

           select input-file assign    to '../../data/lab9.data'
               organization is line sequential.

           select report-file assign   to '../../data/lab9.out'
               organization is line sequential.

       data division.
       file section.

      *input file and record
       fd input-file 
           data record is emp-rec.
       
       01 emp-rec.
           05 emp-rec-num               pic x(3).
           05 emp-rec-name              pic x(12).
           05 emp-rec-calls             occurs 6 times.
               10 emp-calls-p-month     pic 9(3).

       fd report-file 
           data record is print-line.

       01 print-line                    pic x(132).

       working-storage section.
       
      *all the constants used in program
       01 ws-constants.
           05 ws-number-of-months       pic 99   value 6.
          
       01 ws-found-eof                  pic x    value 'n'.
           88 ws-is-end-of-file                  value "y".

      *totals 
       01 ws-totals.
           05 ws-grand-total            pic 9(5) value 0.
           05 ws-emp-total              pic 9(4) value 0.
           05 ws-total-no-calls         pic 9(2) value 0.

      *first header line of the program
       01 ws-name-line.
           05 filler                    pic x(2) value spaces.
           05 filler                    pic x(29)
                                     value 'Shreeji Patel, lab 9'.
           05 filler                    pic x(5)  value spaces.
           05 ws-name-line-date         pic 9(6).
           05 filler                    pic x(5)  value spaces.
           05 ws-name-line-time         pic 9(8).

      *report heading
       01 ws-report-heading.
           05 filler                    pic x(20).
           05 filler                    pic x(39)
                      value 'call centre volumes for july - december'.
       
      *heading columns
       01 ws-heading-line1.
           05 filler                    pic x(2) value spaces.
           05 filler                    pic x(8) value 'operator'.
           05 filler                    pic x(2) value spaces.
           05 filler                    pic x(8) value 'operator'.
           05 filler                    pic x(6) value spaces.
           05 filler                    pic x(3) value 'jul'.
           05 filler                    pic x(4) value spaces.
           05 filler                    pic x(3) value 'aug'.
           05 filler                    pic x(4) value spaces.
           05 filler                    pic x(3) value 'sep'.
           05 filler                    pic x(4) value spaces.
           05 filler                    pic x(3) value 'oct'.
           05 filler                    pic x(4) value spaces.
           05 filler                    pic x(3) value 'nov'.
           05 filler                    pic x(4) value spaces.
           05 filler                    pic x(3) value 'dec'.
           05 filler                    pic x(4) value spaces.
           05 filler                    pic x(5) value 'total'.
           05 filler                    pic x(4) value spaces.
           05 filler                    pic x(3) value 'avg'.
           05 filler                    pic x(6) value spaces.
           05 filler                    pic x(3) value 'rem'.

       01 ws-heading-line2.
           05 filler                    pic x(5) value spaces. 
           05 filler                    pic x(1) value '#'.
           05 filler                    pic x(8) value spaces.
           05 filler                    pic x(4) value 'name'.
       
      *output detail line
       01 ws-detail-line.
           05 filler                    pic x(4) value spaces.
           05 ws-detail-line-num        pic x(3).
           05 filler                    pic x(6) value spaces.
           05 ws-detail-line-name       pic x(12).
           05 filler                    pic x(1) value spaces.
           05 ws-detail-line-months     occurs 6 times.
               10 ws-detail-calls       pic zz9.
               10 ws-spaces             pic x(4) value spaces.
           05 filler                    pic x(1) value spaces.
           05 ws-detail-line-total      pic zz9.
           05 filler                    pic x(5) value spaces.
           05 ws-detail-line-avg        pic zzz9.
           05 ws-detail-line-avg-text   redefines ws-detail-line-avg
                                        pic x(4).
           05 filler                    pic x(4) value spaces.
           05 ws-detail-line-rem        pic zzz9.
           05 ws-detail-line-rem-text   redefines ws-detail-line-rem
                                        pic xxxx.
      *subscript for array
       01 ws-months-str                 pic 99 value 1.

      *line for totals
       01 ws-total-line1.
           05 filler                    pic x(6) value spaces.
           05 filler                    pic x(38)
                          value "number of operators with no calls: ".
           05 ws-total-line-no-calls    pic z9.

       01 ws-total-line2.
           05 filler                    pic x(6) value spaces.
           05 filler                    pic x(20)
               value "overall total calls:".
           05 filler                    pic x(15) value spaces.
           05 ws-total-line-calls       pic zzz99.
       

      *variable to hold the count for non zero values
       01 ws-total-non-zeros            pic 99.

      *average calls and remainder
       01 ws-avg-calls                  pic 999
           value 0.
       01 ws-remainder                  pic 99
           value 0.
       
       procedure division.
           
           open input input-file,
                output report-file.

           
           accept ws-name-line-date from date.
           accept ws-name-line-time from time.

           
           perform 000-print-headings.

           
           perform 100-read-input-file.
           perform 200-process-records until ws-is-end-of-file.
                                                                                                                                                                                                                                 
           
           perform 300-print-totals.

           
           close input-file,
                 report-file.
           
           stop run.

      *print headings 
       000-print-headings.
           write print-line from ws-name-line.

           write print-line from ws-report-heading
               after advancing 1 line.

           write print-line from ws-heading-line1
               after advancing 2 lines.
           write print-line from ws-heading-line2
               after advancing 1 line.

       100-read-input-file.
           
           read input-file
               at end move 'y'         to ws-found-eof.

       200-process-records.
      
           move 0                      to ws-remainder.
           move 0                      to ws-total-non-zeros.
           move 0                      to ws-avg-calls.
           
           
           perform 280-process-record-line.
           
       280-process-record-line.
           perform varying ws-months-str from 1 by 1
               until ws-months-str > ws-number-of-months

      *count only if the value is nonzero       
               if (emp-calls-p-month (ws-months-str)
                   is not equal        to 0) then
                   add 1               to ws-total-non-zeros
               else 
                   add 0               to ws-total-non-zeros
               end-if

      *move the values to the output field and add for the totals
           move emp-calls-p-month (ws-months-str)
                                       to
                                       ws-detail-calls (ws-months-str)
           add emp-calls-p-month (ws-months-str)
                                       to ws-emp-total
           end-perform.

           move emp-rec-num            to ws-detail-line-num.
           move emp-rec-name           to ws-detail-line-name.
           move ws-emp-total           to ws-detail-line-total.
           add ws-emp-total            to ws-grand-total.

           if (ws-total-non-zeros > 0)

               divide ws-emp-total by ws-total-non-zeros
                   giving ws-avg-calls remainder ws-remainder
           end-if.
           
           if(ws-avg-calls = 0) then
               move "ZERO"             to ws-detail-line-avg-text 
               move spaces             to ws-detail-line-rem-text 
               add 1                   to ws-total-no-calls
           else
                move ws-avg-calls      to ws-detail-line-avg
                move ws-remainder      to ws-detail-line-rem
           end-if.
           
           write print-line from ws-detail-line
               after advancing 2 lines.

           
           move 0                      to ws-emp-total.

           
           perform 100-read-input-file.

      *the totals line
       300-print-totals.
       
           move ws-total-no-calls      to ws-total-line-no-calls.
           move ws-grand-total         to ws-total-line-calls.
           
           write print-line from ws-total-line1
               after advancing 2 lines.
           write print-line from ws-total-line2
               after advancing 2 lines.

       

       end program Lab9CallCenterOperatorReport.
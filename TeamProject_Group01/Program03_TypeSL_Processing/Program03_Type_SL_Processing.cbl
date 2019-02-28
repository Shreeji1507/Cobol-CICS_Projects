       identification division.
       program-id. Program03_Type_SL_Processing.
       author. Ryan Beckett, Mathew Kosterzewa, Shreeji Patel.
       Date-Written. 16-Apr-2018.
      *Purpose     : This file is created for the purpose of the group
      *              project in MAFD-4202.
      *Description : This file processes the S&L type transactions and
      *              prints the details 

       environment division.

       input-output section.
       file-control.
      * both input and output files are configured
       select input-file  assign           to
                                       "../../../data/project1S&L.out"
           organization is line sequential.

       select output-file assign           to
                                   "../../../data/project1S&L-Report.out"
           organization is line sequential.

       data division.
       file section.
      *
       fd input-file 
           data record is input-line
               record contains 37 characters.
      *
       01 input-line.
           05 il-trans-code                pic x.
           05 il-trans-amount              pic 9(5)v99.
           05 il-pay-type                  pic xx.
           05 il-store-num                 pic xx.
           05 il-invoice-num.
               10 il-invoice-alpha-1       pic x.
               10 il-invoice-alpha-2       pic x.
               10 il-invoice-sep           pic x.
               10 il-valid-inv-num         pic 9(6).
           05 il-sku-code                  pic x(15).

      *output record
       fd output-file 
           data record is output-line
               record contains 150 characters.
       01 output-line                      pic x(150)
           value spaces.

       working-storage section.

      *detail line
       01 ws-output-detail-line.
           05 filler                       pic x(5)
               value spaces.
           05 ws-ol-trans-code             pic x
               value spaces.
           05 filler                       pic x(7)
               value spaces.
           05 ws-ol-trans-amt              pic $$,$$9.99
               value 0.
           05 filler                       pic x(6)
               value spaces.
           05 ws-ol-pay-type               pic xx
               value spaces.
           05 filler                       pic x(8)
               value spaces.
           05 ws-ol-str-num                pic 99
               value 0.
           05 filler                       pic x(5)
               value spaces.
           05 ws-ol-inv-num                pic x(9)
               value spaces.
           05 filler                       pic x(3)
               value spaces.
           05 ws-ol-sku-code               pic x(17)
               value spaces.
           05 ws-ol-tax-owed               pic $$,$$9.99
               value 0.
      *report header
       01 ws-report-header.
           05 filler                       pic x(2)
               value spaces.
           05 ws-date                      pic 99/99/99
               value 0.
           05 filler                       pic x(4)
               value spaces.
           05 ws-time                      pic 99b99b99b99
               value 0.
           05 filler                       pic x(8)
               value spaces.
           05 filler                       pic x(33)
               value "SALES REPORT".
           
           05 filler                       pic x(5)
              value "PAGE  ".
           05 ws-rh-current-page           pic z9
              value 1.

      *column headings
       01 ws-report-headings-1.
           05 filler                       pic x(1)
               value spaces.
           05 filler                       pic x(13)
               value "TRANSACTION".
           05 filler                       pic x(13)
               value "TRANSACTION".
           05 filler                       pic x(9)
               value "PAYMENT".
           05 filler                       pic x(10)
               value "STORE".
           05 filler                       pic x(16)
               value "INVOICE".
           05 filler                       pic x(17)
               value "SKU".
           05 filler                       pic x(10)
               value "TAX".

        01 ws-report-headings-2.
           05 filler                       pic x(4)
               value spaces.
           05 filler                       pic x(12)
               value "CODE".
           05 filler                       pic x(11)
               value "AMOUNT".
           05 filler                       pic x(9)
               value "TYPE".
           05 filler                       pic x(10)
               value "NUMBER".
           05 filler                       pic x(16)
               value "NUMBER".
           05 filler                       pic x(16)
               value "CODE".
           05 filler                       pic x(10)
               value "OWING".

      *underlines
       01 ws-underlines.
           05 filler                       pic x(1)
               value spaces.
           05 filler                       pic x(13)
               value "-----------".
           05 filler                       pic x(13)
               value "-----------".
           05 filler                       pic x(9)
               value "-------  ".
           05 filler                       pic x(10)
               value "------".
           05 filler                       pic x(16)
               value "-------".
           05 filler                       pic x(16)
               value "----".
           05 filler                       pic x(14)
               value "-----".

      *total records line
       01 ws-total-line-overall.
           05 filler                       pic x
               value spaces.
           05 filler                       pic x(27)
               value "TOTAL RECORDS =".
           05 ws-tl-total-records          pic zz9
               value 0.
           05 filler                       pic x(5)
               value spaces.
           05 filler                       pic x(25)
               value "TOTAL TRANS-AMOUNT =".
           05 ws-tl-total-trans-amt        pic $zzz,zz9.99
               value 0.

       01 ws-total-line-overall-2.
           05 filler                       pic x(36)
               value spaces.
           05 filler                       pic x(25)
               value "TOTAL TAX OWING =".
           05 ws-tl-total-tax-owed         pic $zzz,zz9.99
               value 0.

       01 ws-total-line-l.
           05 filler                       pic x
               value spaces.
           05 filler                       pic x(28)
               value "TOTAL 'L' RECORDS =".
           05 ws-tl-total-l-records        pic z9
               value 0.
           05 filler                       pic x(5)
               value spaces.
           05 filler                       pic x(25)
               value "TOTAL TRANS-AMOUNT(L) =".
           05 ws-tl-total-l-trans-amt      pic $zzz,zz9.99
               value 0.

      *total S type records
       01 ws-total-line-s.
           05 filler                       pic x
               value spaces.
           05 filler                       pic x(28)
               value "TOTAL 'S' RECORDS =".
           05 ws-tl-total-s-records        pic z9
               value 0.
           05 filler                       pic x(5)
               value spaces.
           05 filler                       pic x(25)
               value "TOTAL TRANS-AMOUNT(R) =".
           05 ws-tl-total-s-trans-amt      pic $zzz,zz9.99
               value 0.

      *total credit type records 
       01 ws-perc-line-cr.
           05 filler                       pic x
               value spaces.
           05 filler                       pic x(25)
               value "CREDIT(%) =".
           05 ws-pl-cr-perc                pic z9.99
               value 0.
           05 filler                       pic x
               value "%".

      *total cash type records    
       01 ws-perc-line-ca.
           05 filler                       pic x
               value spaces.
           05 filler                       pic x(25)
               value "CASH(%)   =".
           05 ws-pl-ca-perc                pic z9.99
               value 0.
           05 filler                       pic x
               value "%".

      *total debit type records
       01 ws-perc-line-db.
           05 filler                       pic x
               value spaces.
           05 filler                       pic x(25)
               value "DEBIT(%)  =".
           05 ws-pl-db-perc                pic z9.99
               value 0.
           05 filler                       pic x
               value "%".

      *highest transaction store
       01 ws-highest-trans-str-line-1.
           05 filler                       pic x
               value spaces.
           05 filler                       pic x(35)
               value "STORE NUMBER".

       01 ws-highest-trans-str-line-2.
           05 filler                       pic x
               value spaces.
           05 filler                       pic x(28)
               value "WITH HIGHEST TRANSACTION =".
           05 ws-hts-str-01                pic xx
               value spaces.
        
      *array for highest sales
       01 ws-highest OCCURS 102 times indexed by ws-vt-index.
           05 ws-highest-store-num         pic xx.
           05 ws-highest-amt-store         pic 9(5)v99.
       
      *variables to find the highest amount for given store number
       01 ws-highest-str-num               pic xx
           value spaces.
       01 ws-highest-trans-amt             pic 9(5)v99
           value 0.

       01 ws-eof                           pic x
           value 'n'.

      *line and page counts
       01 ws-line-count                    pic 99
           value 0.

       01 ws-page-count                    pic 99
           value 1.

      *total records, amount and tax for each class and overall
       01 ws-total-s-records               pic 999
           value 0.

       01 ws-total-l-records               pic 999
           value 0.

       01 ws-total-s-trans-amt             pic 9(5)v99
           value 0.

       01 ws-total-l-trans-amt             pic 9(5)v99
           value 0.

       01 ws-total-records                 pic 999
           value 0.

       01 ws-total-trans-amt               pic 9(5)v99
           value 0.

       01 ws-total-tax-owing               pic 9(4)v99
           value 0.

       01 ws-tax-owed                      pic 99999v99
           value 0.

       01 ws-total-s-tax-owing             pic 9(5)v99
           value 0.

       01 ws-total-l-tax-owing              pic 9(5)v99
           value 0.

      *variables for different pay type counts
       01 ws-total-ca-count                pic 999
           value 0.

       01 ws-total-cr-count                pic 999
           value 0.

       01 ws-total-db-count                pic 999
           value 0.

       01 ws-new-line-cnt                  pic 999
           value 0.
       
      *variables for percent value
       01 ws-cr-perc                       pic 99v999
           value 0.

       01 ws-ca-perc                       pic 99v999  
           value 0.

       01 ws-db-perc                       pic 99v999  
           value 0.

      *constants for lines per page, rate and rate multiplier
       77 ws-lines-per-page                pic 999
           value 20.

       77 ws-const-tax-rate                pic 9v99
           value 0.13.

       77 ws-rate-multiplier               pic 999
           value 100.

       77 ws-array-size                    pic 999 
           value 102.

       procedure division.
           open input input-file
                output output-file.

          read input-file
               at end move "y"             to ws-eof.
           
          perform until ws-eof = "y"
               perform 100-report-heading

      *20 lines per page        
                perform 000-process-records
                   varying ws-line-count   from 0 by 1 
                       until ws-line-count = ws-lines-per-page
                           or ws-eof = "y"
            
           end-perform.

      *total values     
           perform 300-total-values.

      *percentage values
           perform 400-percentage-calc.
     
      *totals and percentage lines
           perform 500-report-footer-details.

           perform varying ws-new-line-cnt from 1 by 1 
               until ws-new-line-cnt = ws-array-size

      *loop through the array
               if ws-highest-trans-amt <
                   ws-highest-amt-store (ws-new-line-cnt) then
                    move ws-highest-amt-store (ws-new-line-cnt)
                                           to ws-highest-trans-amt
                    move ws-highest-store-num (ws-new-line-cnt)
                                           to ws-highest-str-num
                end-if
           end-perform.

           move ws-highest-str-num         to ws-hts-str-01.

           write output-line               from
                                           ws-highest-trans-str-line-1
               after advancing 1 line.
           write output-line               from
                                           ws-highest-trans-str-line-2.

           close input-file output-file.
           goback.

      *main processing records     
       000-process-records.
      *move regular values from input line to the output line
           move 0                          to ws-tax-owed.
           move 0                          to ws-ol-tax-owed.
           add 1                           to ws-total-records.
           move il-trans-code              to ws-ol-trans-code.
           move il-trans-amount            to ws-ol-trans-amt.
           add il-trans-amount             to ws-total-trans-amt.
           move il-pay-type                to ws-ol-pay-type.
           move il-invoice-num             to ws-ol-inv-num.
           move il-store-num               to ws-ol-str-num.
           move il-sku-code                to ws-ol-sku-code.

      *if the code is "S" then add to the add specifics
           if (il-trans-code = 'S') then
               add 1                       to ws-total-s-records
               add il-trans-amount         to ws-total-s-trans-amt
               compute ws-tax-owed rounded =
                   il-trans-amount * ws-const-tax-rate
               move ws-tax-owed            to ws-ol-tax-owed
               add ws-tax-owed             to ws-total-s-tax-owing
      *else "L"
           else
               add il-trans-amount         to ws-total-l-trans-amt
               add 1                       to ws-total-l-records
               compute ws-tax-owed rounded =
                   il-trans-amount * ws-const-tax-rate
               move ws-tax-owed            to ws-ol-tax-owed
               add ws-tax-owed             to ws-total-l-tax-owing
           end-if.

      *calculate total values        
           compute ws-total-tax-owing =
               ws-total-l-tax-owing + ws-total-s-tax-owing.
           compute ws-total-trans-amt =
               ws-total-l-trans-amt + ws-total-s-trans-amt.
           perform 200-pay-type-calc

           set ws-vt-index up by 1
           move il-trans-amount            to
               ws-highest-amt-store (ws-vt-index).
           move il-store-num               to
               ws-highest-store-num (ws-vt-index).
           write output-line               from ws-output-detail-line.

           read input-file
                   at end move "y"         to ws-eof.

      *report heading
       100-report-heading.
           accept ws-date                  from date.
           accept ws-time                  from time.
           write output-line               from spaces.
           write output-line               from spaces.
           write output-line               from ws-report-header
               after advancing page.
           add 1                           to ws-page-count.
           move ws-page-count              to ws-rh-current-page.
           write output-line               from spaces.
           write output-line               from spaces.
           write output-line               from ws-report-headings-1.
           write output-line               from ws-report-headings-2.
           write output-line               from ws-underlines.

      *different pay type   
       200-pay-type-calc.
           if (il-pay-type = 'CA')
               add 1                       to ws-total-ca-count
           else
               if (il-pay-type = 'CR') then
                   add 1                   to ws-total-cr-count
               else
                   add 1                   to ws-total-db-count
               end-if
           end-if.

      *total values
       300-total-values.
           move ws-total-records           to ws-tl-total-records.
           move ws-total-l-records         to ws-tl-total-l-records.
           move ws-total-s-records         to ws-tl-total-s-records.
           move ws-total-trans-amt         to ws-tl-total-trans-amt.
           move ws-total-l-trans-amt       to ws-tl-total-l-trans-amt.
           move ws-total-s-trans-amt       to ws-tl-total-s-trans-amt.
           move ws-total-tax-owing         to ws-tl-total-tax-owed.

      *pecentage count for each payment type
       400-percentage-calc.
           compute ws-cr-perc rounded =
               (ws-total-cr-count / ws-total-records) *
                   ws-rate-multiplier.
           compute ws-ca-perc rounded =
               (ws-total-ca-count / ws-total-records) *
                   ws-rate-multiplier.
           compute ws-db-perc rounded =
               (ws-total-db-count / ws-total-records) *
                   ws-rate-multiplier.

      *write all details at the bottom of the report
       500-report-footer-details.
           move ws-ca-perc                 to ws-pl-ca-perc.
           compute ws-pl-db-perc rounded = ws-db-perc.
           move ws-cr-perc                 to ws-pl-cr-perc.
          
           write output-line               from spaces.
           write output-line               from spaces.
           write output-line               from ws-total-line-overall.
           write output-line               from
                                           ws-total-line-overall-2.
           write output-line               from ws-total-line-l
               after advancing 1 line.
           write output-line               from ws-total-line-s. 

           write output-line               from ws-perc-line-cr
               after advancing 1 line.
           write output-line               from ws-perc-line-ca.
           write output-line               from ws-perc-line-db.
       end program Program03_Type_SL_Processing.

       identification division.
       program-id. Program04_Type_R_Processing.
       author. Shreeji Patel, Ryan Beckett, Mathew Kosterzewa.
       Date-Written. 15-Apr-2018.
      *Purpose     : This file is created for the purpose of the group
      *              project in MAFD-4202.
      *Description : This file creates the report for the returns.

       environment division.

       input-output section.
       file-control.
      * both input and output files are configured
           select input-file  assign       to
                                     "../../../data/project1Return.out"
               organization is line sequential.

           select output-file assign       to
                                   "../../../data/project1R-Report.out"
               organization is line sequential.

       data division.
       file section.
      
       fd input-file 
           data record is input-line
               record contains 37 characters.
      
       01 input-line.
           05 il-trans-code                pic x.
           05 il-trans-amount              pic 9(5)v99.
           05 il-pay-type                  pic xx.
           05 il-store-num                 pic xx.
           05 il-invoice-num               pic x(9).
           05 il-sku-code                  pic x(15). 
      
       fd output-file 
           data record is output-line
               record contains 100 characters.
       01 output-line                      pic x(100)
           value spaces.

       working-storage section.

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
           05 ws-ol-sku-code               pic x(18)
               value spaces.
           05 ws-ol-tax-owed               pic $$9.99
               value 0.

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
               value "RETURNS REPORT".
           
           05 filler                       pic x(5)
              value "PAGE  ".
           05 ws-rh-current-page           pic z9
              value 1.

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
           05 filler                       pic x(14)
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
           05 filler                       pic x(14)
               value "CODE".
           05 filler                       pic x(10)
               value "OWED".
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
           05 filler                       pic x(14)
               value "----".
           05 filler                       pic x(14)
               value "----".
           
      *line for totals
       01 ws-total-line-1.
           05 filler                       pic x
               value spaces.
           05 filler                       pic x(28)
               value "TOTAL RECORDS     :".
           05 ws-tl-total-records          pic z9
               value 0.

       01 ws-total-line-2.
           05 filler                       pic x
               value spaces.
           05 filler                       pic x(21)
               value "TOTAL TRANSACTION :".
           05 ws-tl-total-trans            pic $z,zz9.99
               value 0.

       01 ws-total-line-3.
           05 filler                       pic x
               value spaces.
           05 filler                       pic x(21)
               value "TOTAL TAX OWED    :".
           05 ws-tl-total-tax-owed         pic $z,zz9.99
               value 0.

       01 ws-eof                           pic x
           value 'n'.

      *variables used in calculation
       01 ws-line-count                    pic 99
           value 0.
       01 ws-page-count                    pic 99
           value 1.
       01 ws-total-records                 pic 99
           value 0.
       01 ws-total-trans-amt               pic 9(5)v99
           value 0.
       01 ws-total-tax-owed                pic 9(4)v99
           value 0.
       01 ws-tax-owed                      pic 999v99
           value 0.

      *constants for lines per page and tax rate
       77 ws-lines-per-page                pic 99
           value 20.
       77 ws-const-tax-rate                pic 9v99
           value 0.13.

       procedure division.
           open input input-file
                output output-file.

           
           read input-file
               at end move "y"             to ws-eof.
           
           perform until ws-eof = "y"
               perform 100-report-heading
           
                perform 000-process-records
                   varying ws-line-count   from 0 by 1 
                       until ws-line-count = ws-lines-per-page
                           or ws-eof = "y"

           end-perform.

      *move the calculated fields to the record and write the lines
           move ws-total-records           to ws-tl-total-records.
           move ws-total-trans-amt         to ws-tl-total-trans.
           move ws-total-tax-owed          to ws-tl-total-tax-owed.

           write output-line               from ws-total-line-1
               after advancing 2 lines.
           write output-line               from ws-total-line-2
               after advancing 1 line.
           write output-line               from ws-total-line-3
               after advancing 1 line.

           close input-file output-file.
           goback.
           
      *main loop that moves the data to the record and calculate
      *the values
       000-process-records.
           add 1                           to ws-total-records.
           move il-trans-code              to ws-ol-trans-code.
           move il-trans-amount            to ws-ol-trans-amt.
           add il-trans-amount             to ws-total-trans-amt.
           move il-pay-type                to ws-ol-pay-type.
           move il-invoice-num             to ws-ol-inv-num.
           move il-store-num               to ws-ol-str-num.
           move il-sku-code                to ws-ol-sku-code.

           compute ws-tax-owed rounded =
               il-trans-amount * ws-const-tax-rate.
           add ws-tax-owed                 to ws-total-tax-owed.
           move ws-tax-owed                to ws-ol-tax-owed.
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

       end program Program04_Type_R_Processing.

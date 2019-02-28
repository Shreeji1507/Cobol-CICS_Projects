       identification division.
       program-id. Program02_DataSplitCount.
       author. Ryan Beckett, Mathew Kostrzewa, Shreeji Patel.
       Date-Written. 09-Apr-2018.
      *
	   environment division.
	   configuration section.
	   input-output section.
	   file-control.
	  *
	   select valid-file assign            to
                                      "../../../data/project1Valid.out"
	           organization is line sequential.
	   select sales-file assign            to
                                       "../../../data/project1S&L.out"
		       organization is line sequential.
	   select ret-file assign              to
                                    "../../../data/project1Return.out"
		       organization is line sequential.
	   select con-file assign              to
                                   "../../../data/project1Control.out"
		       organization is line sequential.
      *
       data division.
	   file section.
      *
       fd valid-file 
           data record is input-line
               record contains 37 characters.
      *
       01 input-line.
           05 il-trans-code                pic x.
           05 il-trans-amount              pic 9(5)v99.
           05 il-pay-type                  pic xx.
           05 il-store-num                 pic xx.
           05 il-invoice-num               pic x(9).
           05 il-sku-code                  pic x(15). 
      *
	   fd sales-file
	       data record is output-sline.
      *
	   01 output-sline.
           05 filler                       pic x(37)
	           value spaces.
      *
	   fd ret-file
	       data record is output-rline.
      *
	   01 output-rline.
           05 filler                       pic x(37)
	           value spaces.
      *
	   fd con-file
	       data record is output-cline.
      *
	   01 output-cline.
           05 filler                       pic x(80)
	           value spaces.
      *
       working-storage section.
	  *
	   01 ws-rpt-ln1.
	       05 filler                       pic x(37)
		       value "Total number of S&L entries:        ".
		   05 ws-rpt-cnt-sl                pic 999
		       value 0.
      *
	   01 ws-rpt-ln2.
		   05 filler                       pic x(30)
		       value "Total S&L amount:            ".
		   05 ws-rpt-amt-sl                pic $$$,$$9.99
		       value 0.
      *
	   01 ws-rpt-ln3.
	       05 filler                       pic x(38)
		       value "Total number of S entries:           ".
		   05 ws-rpt-cnt-s                 pic 99
		       value 0.
      *
	   01 ws-rpt-ln4.
	       05 filler                       pic x(30)
		       value "Total S amount:              ".
		   05 ws-rpt-amt-s                 pic $$$,$$9.99
		       value 0.
      *
	   01 ws-rpt-ln5.
	       05 filler                       pic x(38)
		       value "Total number of L entries:           ".
		   05 ws-rpt-cnt-l                 pic 99
		       value 0.
      *
	   01 ws-rpt-ln6.
	       05 filler                       pic x(30)
		       value "Total L amount:              ".
		   05 ws-rpt-amt-l                 pic $$$,$$9.99
		       value 0.
      *
	   01 ws-rpt-ln7.
	       05 filler                       pic x(30)
		       value "Total transactions store 01: ".
		   05 ws-rpt-tot-st1               pic $$$,$$9.99
		       value 0.
      *
	   01 ws-rpt-ln8.
	       05 filler                       pic x(30)
		       value "Total transactions store 02: ".
		   05 ws-rpt-tot-st2               pic $$$,$$9.99
		       value 0.
      *
	   01 ws-rpt-ln9.
	       05 filler                       pic x(30)
		       value "Total transactions store 03: ".
		   05 ws-rpt-tot-st3               pic $$$,$$9.99
		       value 0.
      *
	   01 ws-rpt-ln10.
	       05 filler                       pic x(30)
		       value "Total transactions store 04: ".
		   05 ws-rpt-tot-st4               pic $$$,$$9.99
		       value 0.
      *
	   01 ws-rpt-ln11.
	       05 filler                       pic x(35)
		       value "Payments Cash:                   %".
		   05 ws-rpt-pmt-ca                pic 99.99
		       value 0.
      *
	   01 ws-rpt-ln12.
	       05 filler                       pic x(35)
		       value "Payments Credit:                 %".
		   05 ws-rpt-pmt-cr                pic 99.99
		       value 0.
      *
	   01 ws-rpt-ln13.
	       05 filler                       pic x(35)
		       value "Payments Debit:                  %".
		   05 ws-rpt-pmt-db                pic 99.99
		       value 0.
      *
	   01 ws-rpt-ln14.
	       05 filler                       pic x(38)
		       value "Total Returns:                       ".
		   05 ws-rpt-cnt-ret               pic 99
		       value 0.
      *
	   01 ws-rpt-ln15.
	       05 filler                       pic x(30)
		       value "Total Returns Amount:        ".
		   05 ws-rpt-amt-ret               pic $$$,$$9.99
		       value 0.
      *
	   01 ws-rpt-ln16.
	       05 filler                       pic x(30)
		       value "Return Amount Store 01:      ".
		   05 ws-rpt-amt-st1               pic $$$,$$9.99
		       value 0.
      *
	   01 ws-rpt-ln17.
	       05 filler                       pic x(30)
		       value "Return Amount Store 02:      ".
		   05 ws-rpt-amt-st2               pic $$$,$$9.99
		       value 0.
      *
	   01 ws-rpt-ln18.
	       05 filler                       pic x(30)
		       value "Return Amount Store 03:      ".
		   05 ws-rpt-amt-st3               pic $$$,$$9.99
		       value 0.
      *
	   01 ws-rpt-ln19.
	       05 filler                       pic x(30)
		       value "Return Amount Store 07:      ".
		   05 ws-rpt-amt-st4               pic $$$,$$9.99
		       value 0.
      *
	   01 ws-rpt-ln20.
	       05 filler                       pic x(29)
		       value "Grand Total:                ".
		   05 ws-rpt-gr-tot                pic $$$$,$$9.99
		       value 0.
      *
	   77 ws-rate-multiplier               pic 999
           value 100.
	   01 ws-eof                           pic x
	       value 'n'.
	   01 ws-cnt-s                         pic 999
	       value 0.
	   01 ws-tot-s                         pic 9(6)v99
	       value 0.
	   01 ws-cnt-l                         pic 99
	       value 0.
	   01 ws-tot-l                         pic 9(6)v99
	       value 0.
	   01 ws-cnt-r                         pic 99
	       value 0.
	   01 ws-tot-r                         pic 9(6)v99
	       value 0.
	   01 ws-tot-st-1                      pic 9(6)v99
	       value 0.
	   01 ws-tot-st-2                      pic 9(6)v99
	       value 0.
	   01 ws-tot-st-3                      pic 9(6)v99
	       value 0.
	   01 ws-tot-st-4                      pic 9(6)v99
	       value 0.
	   01 ws-cnt-ca                        pic 99
	       value 0.
	   01 ws-cnt-cr                        pic 99
	       value 0.
	   01 ws-cnt-db                        pic 99
	       value 0.
	   01 ws-totr-st-1                     pic 9(6)v99
	       value 0.
	   01 ws-totr-st-2                     pic 9(6)v99
	       value 0.
	   01 ws-totr-st-3                     pic 9(6)v99
	       value 0.
	   01 ws-totr-st-4                     pic 9(6)v99
	       value 0.
	   01 ws-tmp                           pic 9(6)v99
	       value 0.
	   01 ws-cnt-all                       pic 999
	       value 0.
	   01 ws-ca-perc                       pic 99v999
	       value 0.
	   01 ws-cr-perc                       pic 99v999
	       value 0.
	   01 ws-db-perc                       pic 99v999
	       value 0.
	  *
      *
       procedure division.
		   open input valid-file.
		   open output sales-file, ret-file, con-file.
           read valid-file at end move 'y' to ws-eof.
      *while loop
		   perform until ws-eof equals 'y'
			   perform 000-trans-code
			   perform 001-store-code
			   perform 002-pymt-type
			   read valid-file
                   at end move 'y'         to ws-eof
		   end-perform.
      *write control file
		   perform 003-prc-totals.
		   close con-file, ret-file, sales-file, valid-file.
           stop run.
      *
	   000-trans-code.
		   evaluate il-trans-code
			   when 'S' perform 100-prc-sale
			   when 'L' perform 200-prc-layfile
			   when 'R' perform 300-prc-retfile
           end-evaluate.
      *
	   001-store-code.
		   evaluate il-store-num
			   when '01' perform 400-prc-1-store
			   when '02' perform 500-prc-2-store
			   when '03' perform 600-prc-3-store
			   when '07' perform 700-prc-4-store
           end-evaluate.
      *
	   002-pymt-type.
		   if not 'R' equals il-trans-code then
		       evaluate il-pay-type
			       when 'CA'
                       perform 800-prc-cncl
			       when 'CR' perform 900-prc-cred
			       when 'DB' perform 1000-prc-deb
               end-evaluate
		   end-if.
      *
       100-prc-sale.
		   add 1                           to ws-cnt-s.
		   add il-trans-amount             to ws-tot-s.
		   move input-line                 to output-sline.
		   write output-sline.
      *
       200-prc-layfile.
		   add 1                           to ws-cnt-l.
		   add il-trans-amount             to ws-tot-l.
		   move input-line                 to output-sline.
		   write output-sline.
      *returns file
	   300-prc-retfile.
		   add 1                           to ws-cnt-r.
		   add il-trans-amount             to ws-tot-r.
		   evaluate il-store-num
			   when '01'
                   add il-trans-amount     to ws-totr-st-1
			   when '02'
                   add il-trans-amount     to ws-totr-st-2
			   when '03'
                   add il-trans-amount     to ws-totr-st-3
			   when '07'
                   add il-trans-amount     to ws-totr-st-4
           end-evaluate.
		   move input-line                 to output-rline.
		   write output-rline.
      *
	   400-prc-1-store.
		   add il-trans-amount             to ws-tot-st-1.
	   500-prc-2-store.
		   add il-trans-amount             to ws-tot-st-2.
       600-prc-3-store.
		   add il-trans-amount             to ws-tot-st-3.
	   700-prc-4-store.
		   add il-trans-amount             to ws-tot-st-4.
	   800-prc-cncl.
		   add 1                           to ws-cnt-ca.
	   900-prc-cred.
		   add 1                           to ws-cnt-cr.
       1000-prc-deb.
		   add 1                           to ws-cnt-db.
	   003-prc-totals.
      *
		   compute ws-tmp = ws-cnt-l + ws-cnt-s.
		   move ws-tmp                     to ws-rpt-cnt-sl.
		   move ws-rpt-ln1                 to output-cline.
           write output-cline.
      *
		   compute ws-tmp = ws-tot-l + ws-tot-s.
		   move ws-tmp                     to ws-rpt-amt-sl.
		   move ws-rpt-ln2                 to output-cline.
		   write output-cline.
      *
		   move ws-cnt-s                   to ws-rpt-cnt-s.
		   move ws-rpt-ln3                 to output-cline.
		   write output-cline.
      *
		   move ws-tot-s                   to ws-rpt-amt-s.
		   move ws-rpt-ln4                 to output-cline.
		   write output-cline.
	  *
           move ws-cnt-l                   to ws-rpt-cnt-l.
		   move ws-rpt-ln5                 to output-cline.
		   write output-cline.
      *
		   move ws-tot-l                   to ws-rpt-amt-l.
		   move ws-rpt-ln6                 to output-cline.
		   write output-cline.
      *
		   move ws-tot-st-1                to ws-rpt-tot-st1.
		   move ws-rpt-ln7                 to output-cline.
		   write output-cline.
      *
		   move ws-tot-st-2                to ws-rpt-tot-st2.
		   move ws-rpt-ln8                 to output-cline.
		   write output-cline.
      *
		   move ws-tot-st-3                to ws-rpt-tot-st3.
		   move ws-rpt-ln9                 to output-cline.
		   write output-cline.
      *
		   move ws-tot-st-4                to ws-rpt-tot-st4.
		   move ws-rpt-ln10                to output-cline.
		   write output-cline.
      *
		   compute ws-cnt-all = ws-cnt-l + ws-cnt-s.
		   if ws-cnt-ca not equal 0 then
			   compute ws-ca-perc rounded =
                 (ws-cnt-ca / ws-cnt-all) *
                                   ws-rate-multiplier
			   move ws-ca-perc             to ws-rpt-pmt-ca
           end-if.
		   move ws-rpt-ln11                to output-cline.
		   write output-cline.
      *
		   if ws-cnt-cr not equal 0 then
			   compute ws-cr-perc rounded =
                 (ws-cnt-cr / ws-cnt-all) *
                                   ws-rate-multiplier
			   move ws-cr-perc             to ws-rpt-pmt-cr
           end-if
		   move ws-rpt-ln12                to output-cline.
		   write output-cline.
      *
		   if ws-cnt-db not equal 0 then
			   compute ws-db-perc rounded =
                 (ws-cnt-db / ws-cnt-all) *
                                   ws-rate-multiplier
               compute ws-rpt-pmt-db rounded = ws-db-perc
           end-if
		   move ws-rpt-ln13                to output-cline.
		   write output-cline.
      *
		   move ws-cnt-r                   to ws-rpt-cnt-ret.
		   move ws-rpt-ln14                to output-cline.
		   write output-cline.
      *
		   move ws-tot-r                   to ws-rpt-amt-ret.
		   move ws-rpt-ln15                to output-cline.
		   write output-cline.
      *
		   move ws-totr-st-1               to ws-rpt-amt-st1.
		   move ws-rpt-ln16                to output-cline.
		   write output-cline.
      *
		   move ws-totr-st-2               to ws-rpt-amt-st2.
		   move ws-rpt-ln17                to output-cline.
		   write output-cline.
      *
		   move ws-totr-st-3               to ws-rpt-amt-st3.
		   move ws-rpt-ln18                to output-cline.
		   write output-cline.
      *
		   move ws-totr-st-4               to ws-rpt-amt-st4.
		   move ws-rpt-ln19                to output-cline.
		   write output-cline.
      *
		   move ws-rpt-amt-sl              to ws-rpt-gr-tot.
		   move ws-rpt-ln20                to output-cline.
		   write output-cline.
      *
       end program Program02_DataSplitCount.


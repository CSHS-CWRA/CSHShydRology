context('Testing PadPot')

test_that('Verifying PadPot',{

	## remove incomplete year
	xdate <- seq(as.Date('2000/11/1'), as.Date('2003/12/31'), 'days')
	xd1 <- data.frame(d = xdate,
										v = seq_along(xdate))

	xout1 <- PadPot(xd1)
	expect_equal(dim(xout1), c(1095,2))
	expect_equivalent(xout1, xd1[62:nrow(xd1),])
	expect_equal(colnames(xout1), colnames(xd1))

	## Pad complete year
	xdate <- seq(as.Date('2000/1/10'), as.Date('2001/12/31'), 'days')
	xd2 <- data.frame(d = xdate,
										v = seq_along(xdate))

	xout2 <- PadPot(xd2)
	expect_equal(xout2[1:9,2], rep(0,9))

	## Verify multiple sites
	xd3 <- rbind(data.frame(s = 'S1', xd1), data.frame(s = 'S2', xd2))
	xref3 <- rbind(data.frame(s = 'S1', xout1), data.frame(s = 'S2', xout2))

	xout3 <- PadPot(xd3)
	expect_equivalent(xout3,xref3)
	
	## testing formula
	xout3 <- PadPot(v~s+d, xd3)
	expect_equivalent(xout3,xref3)
	
	xout4 <- PadPot(v~d, xd2)
	expect_equivalent(xout2,xout4)
	

})
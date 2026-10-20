test_that("essCW matches a hand-computed example", {
  # Level 1 items at 400, 410, 420 and an unexpectedly hard one at 500;
  # Level 2 items at 440, 450, 460.
  d <- data.frame(location = c(400, 410, 420, 440, 450, 460, 500),
                  ALD      = c(1,   1,   1,   2,   2,   2,   1))
  prof <- EmStanS:::essCW(d, cut_level = 2)$incon_default

  expect_equal(prof$cut_score, d$location)
  expect_equal(prof$counts,  c(4, 3, 2, 1, 2, 3, 4))
  expect_equal(prof$weights, c(130, 100, 80, 60, 60, 70, 150))
})

test_that("essCW matches the brute-force definition, including ties", {
  set.seed(1)
  for (i in 1:200) {
    n   <- sample(c(3, 15, 60), 1)
    nl  <- sample(2:5, 1)
    loc <- if (i %% 2) round(rnorm(n, 500, 30)) else rnorm(n)   # ties when rounded
    ald <- sample(seq_len(nl), n, replace = TRUE)
    for (l in 2:nl) {
      got <- EmStanS:::essCW(data.frame(location = loc, ALD = ald), cut_level = l)$incon_default
      ref <- reference_profile(loc, ald, l)
      expect_equal(got$counts,  unname(ref[, "count"]))
      expect_equal(got$weights, unname(ref[, "weight"]), tolerance = 1e-10)
    }
  }
})

test_that("selection picks lowest weight, then lowest count", {
  # Weight ties at 440 and 450 (both 60); 440 has the lower count (1 vs 2).
  prof <- data.frame(L2_C = c(4, 3, 2, 1, 2, 3, 4),
                     L2_W = c(130, 100, 80, 60, 60, 70, 150))
  cp <- EmStanS:::cal_minp(prof)
  expect_equal(EmStanS:::select_cp(cp, prof, WESS = TRUE, gamest = FALSE), 4)
  # Count: unique minimum at row 4 as well.
  expect_equal(EmStanS:::select_cp(cp, prof, WESS = FALSE, gamest = FALSE), 4)
})

test_that("minimums are exact, not taken after rounding to 2 decimals", {
  # Weights that differ only in the third decimal: rounding to 2 decimals
  # would tie them and fall back to the first row.
  prof <- data.frame(L2_C = c(1, 1, 1), L2_W = c(0.004, 0.001, 0.003))
  prof <- EmStanS:::exact_profile(prof)
  cp <- EmStanS:::cal_minp(prof)
  expect_equal(EmStanS:::select_cp(cp, prof, WESS = TRUE, gamest = FALSE), 2)
})

test_that("exact_profile restores ties broken by floating-point noise", {
  prof <- data.frame(L2_C = c(2, 1), L2_W = c(0.3, 0.1 + 0.2))  # 0.1 + 0.2 != 0.3
  expect_false(prof$L2_W[1] == prof$L2_W[2])
  prof <- EmStanS:::exact_profile(prof)
  cp <- EmStanS:::cal_minp(prof)
  # Tied on weight, so the lower count (row 2) wins rather than noise.
  expect_equal(EmStanS:::select_cp(cp, prof, WESS = TRUE, gamest = FALSE), 2)
})

{
  (* --- Accuracy anchors: 10 stars × RA + Dec = 20 tests, tolerance 0.1 deg --- *)
  VerificationTest[Abs[cnStarRADec["Sirius"][[1]] - 101.2872] < 0.1, True, TestID -> "star-ra-sirius"],
  VerificationTest[Abs[cnStarRADec["Sirius"][[2]] - (-16.7161)] < 0.1, True, TestID -> "star-dec-sirius"],

  VerificationTest[Abs[cnStarRADec["Vega"][[1]] - 279.2347] < 0.1, True, TestID -> "star-ra-vega"],
  VerificationTest[Abs[cnStarRADec["Vega"][[2]] - 38.7837] < 0.1, True, TestID -> "star-dec-vega"],

  VerificationTest[Abs[cnStarRADec["Polaris"][[1]] - 37.9529] < 0.1, True, TestID -> "star-ra-polaris"],
  VerificationTest[Abs[cnStarRADec["Polaris"][[2]] - 89.2641] < 0.1, True, TestID -> "star-dec-polaris"],

  VerificationTest[Abs[cnStarRADec["Arcturus"][[1]] - 213.9154] < 0.1, True, TestID -> "star-ra-arcturus"],
  VerificationTest[Abs[cnStarRADec["Arcturus"][[2]] - 19.1822] < 0.1, True, TestID -> "star-dec-arcturus"],

  VerificationTest[Abs[cnStarRADec["Betelgeuse"][[1]] - 88.7929] < 0.1, True, TestID -> "star-ra-betelgeuse"],
  VerificationTest[Abs[cnStarRADec["Betelgeuse"][[2]] - 7.4071] < 0.1, True, TestID -> "star-dec-betelgeuse"],

  VerificationTest[Abs[cnStarRADec["Rigel"][[1]] - 78.6345] < 0.1, True, TestID -> "star-ra-rigel"],
  VerificationTest[Abs[cnStarRADec["Rigel"][[2]] - (-8.2016)] < 0.1, True, TestID -> "star-dec-rigel"],

  VerificationTest[Abs[cnStarRADec["Canopus"][[1]] - 95.9879] < 0.1, True, TestID -> "star-ra-canopus"],
  VerificationTest[Abs[cnStarRADec["Canopus"][[2]] - (-52.6957)] < 0.1, True, TestID -> "star-dec-canopus"],

  VerificationTest[Abs[cnStarRADec["Capella"][[1]] - 79.1723] < 0.1, True, TestID -> "star-ra-capella"],
  VerificationTest[Abs[cnStarRADec["Capella"][[2]] - 45.9980] < 0.1, True, TestID -> "star-dec-capella"],

  VerificationTest[Abs[cnStarRADec["Antares"][[1]] - 247.3519] < 0.1, True, TestID -> "star-ra-antares"],
  VerificationTest[Abs[cnStarRADec["Antares"][[2]] - (-26.4320)] < 0.1, True, TestID -> "star-dec-antares"],

  VerificationTest[Abs[cnStarRADec["Spica"][[1]] - 201.2983] < 0.1, True, TestID -> "star-ra-spica"],
  VerificationTest[Abs[cnStarRADec["Spica"][[2]] - (-11.1613)] < 0.1, True, TestID -> "star-dec-spica"],

  (* --- Completeness --- *)
  VerificationTest[Length[cnLoadStars[]] >= 57, True, TestID -> "star-catalogue-completeness"],

  (* --- GP latitude equals precessed declination (not J2000 — cnBodyGP now precesses) --- *)
  VerificationTest[
    With[{t = DateObject[{2025, 6, 21, 12, 0, 0}, TimeZone -> 0]},
      Abs[cnBodyGP[cnStarRADec["Sirius"], t][[1]] -
          cnPrecess[cnStarRADec["Sirius"], t][[2]]] < 1*^-6],
    True, TestID -> "star-gp-lat-equals-dec"],

  (* --- Self-consistency: body at its own GP is at zenith --- *)
  VerificationTest[
    With[{t = DateObject[{2025, 6, 21, 12, 0, 0}, TimeZone -> 0],
          radec = cnStarRADec["Sirius"]},
      Abs[cnAltitudeFromGP[cnBodyGP[radec, t], cnBodyGP[radec, t]] - 90] < 1*^-4],
    True, TestID -> "star-gp-zenith-self-consistency"],

  (* --- Two bodies 90 deg apart in RA have GP longitudes 90 deg apart --- *)
  VerificationTest[
    Module[{t = DateObject[{2025, 6, 21, 12, 0, 0}, TimeZone -> 0],
            ra1, ra2, lon1, lon2, diff},
      ra1 = cnStarRADec["Sirius"][[1]];
      ra2 = ra1 + 90.0;
      lon1 = cnBodyGP[{ra1, 0.}, t][[2]];
      lon2 = cnBodyGP[{ra2, 0.}, t][[2]];
      diff = Mod[lon2 - lon1 - 90, 360];
      Abs[diff] < 0.01 || Abs[diff - 360] < 0.01],
    True, TestID -> "star-gp-ra-separation"],

  VerificationTest[Quiet[cnStarRADec["NoSuchStar"]], $Failed, TestID -> "star-missing-name-fails-loud"]
}

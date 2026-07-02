With[{t = DateObject[{2024, 11, 15, 14, 30, 0}, TimeZone -> 0]},
{
  (* Verified earlier: altitude at {20,-40} mid-Atlantic ~ 51.29 deg *)
  VerificationTest[Abs[cnSunAltitude[{20.0, -40.0}, t] - 51.29] < 0.2, True, TestID -> "sun-altitude-known"],
  (* Azimuth near local noon at lon -40 is roughly south (~180 deg) *)
  VerificationTest[150 < cnSunAzimuth[{20.0, -40.0}, t] < 210, True, TestID -> "sun-azimuth-noonish"],
  (* GP latitude equals solar declination, ~ -18.5 deg in mid-Nov *)
  VerificationTest[-20 < cnSunGP[t][[1]] < -17, True, TestID -> "gp-declination"],
  (* THE anchor: at the GP the Sun is at the zenith (altitude 90) *)
  VerificationTest[Abs[cnSunAltitude[cnSunGP[t], t] - 90] < 0.05, True, TestID -> "gp-is-zenith"]
}]

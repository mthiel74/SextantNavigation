{
  (* Dip: 1.76*Sqrt[h]; at 4 m eye height ~ 3.52' *)
  VerificationTest[Abs[cnDip[4.0] - 3.52] < 0.02, True, TestID -> "dip-4m"],
  (* Refraction near horizon is large (~28-34'), at zenith ~0 *)
  VerificationTest[cnRefraction[0.0] > 28 && cnRefraction[0.0] < 40, True, TestID -> "refraction-horizon"],
  VerificationTest[cnRefraction[80.0] < 0.5, True, TestID -> "refraction-high"],
  (* Refraction at 45 deg ~ 0.96' (standard table value) *)
  VerificationTest[Abs[cnRefraction[45.0] - 0.97] < 0.1, True, TestID -> "refraction-45"],
  VerificationTest[cnSunSemidiameter[] == 16.0, True, TestID -> "sd-const"],
  (* Full correction: Hs=45deg, 2m eye, 0 index, lower limb. *)
  (* Ho = 45 - 0 - dip(2.49') - refr(0.97') + sd(16') + plx(~0.11') over 60 *)
  VerificationTest[
    Abs[cnObservedAltitude[45.0, 2.0, 0.0, "Lower"] - (45.0 + (-2.489 - 0.966 + 16.0 + 0.106)/60)] < 0.002,
    True, TestID -> "observed-altitude-lower"]
}

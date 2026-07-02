(* semidiameter_tests.wl — A5: date-varying solar semi-diameter
   cnSunSemidiameterAt[t] scales 16.0' by 1/r where r is the Earth-Sun distance.
   Perihelion (~Jan 3): r ~ 0.9833 AU -> SD ~ 16.27'  (> 16.0')
   Aphelion  (~Jul 5): r ~ 1.0167 AU -> SD ~ 15.74'  (< 16.0')
   Both should be within 15.7-16.3'.
   cnSunSemidiameter[] must still return 16.0 (backward compat).
*)
{
  (* 1. Backward compatibility: constant cnSunSemidiameter[] is 16.0 arcmin. *)
  VerificationTest[cnSunSemidiameter[] == 16.0, True, TestID -> "sd-const-backward-compat"],

  (* 2. Near perihelion (Jan 3): SD > 16.0' *)
  VerificationTest[
    cnSunSemidiameterAt[DateObject[{2024, 1, 3, 12, 0, 0}, TimeZone -> 0]] > 16.0,
    True,
    TestID -> "sd-perihelion-larger"],

  (* 3. Near aphelion (Jul 5): SD < 16.0' *)
  VerificationTest[
    cnSunSemidiameterAt[DateObject[{2024, 7, 5, 12, 0, 0}, TimeZone -> 0]] < 16.0,
    True,
    TestID -> "sd-aphelion-smaller"],

  (* 4. Both values are within the physically realistic range 15.7-16.3'. *)
  VerificationTest[
    With[{sdJan = cnSunSemidiameterAt[DateObject[{2024, 1, 3, 12, 0, 0}, TimeZone -> 0]],
          sdJul = cnSunSemidiameterAt[DateObject[{2024, 7, 5, 12, 0, 0}, TimeZone -> 0]]},
      15.7 < sdJan < 16.3 && 15.7 < sdJul < 16.3],
    True,
    TestID -> "sd-both-in-range"],

  (* 5. Perihelion SD is larger than aphelion SD (direction is correct). *)
  VerificationTest[
    cnSunSemidiameterAt[DateObject[{2024, 1, 3, 12, 0, 0}, TimeZone -> 0]] >
    cnSunSemidiameterAt[DateObject[{2024, 7, 5, 12, 0, 0}, TimeZone -> 0]],
    True,
    TestID -> "sd-perihelion-gt-aphelion"],

  (* 6. Absolute difference between perihelion and aphelion is ~0.5' (0.3-0.6' expected). *)
  VerificationTest[
    With[{sdJan = cnSunSemidiameterAt[DateObject[{2024, 1, 3, 12, 0, 0}, TimeZone -> 0]],
          sdJul = cnSunSemidiameterAt[DateObject[{2024, 7, 5, 12, 0, 0}, TimeZone -> 0]]},
      0.3 < sdJan - sdJul < 0.7],
    True,
    TestID -> "sd-seasonal-range"]
}

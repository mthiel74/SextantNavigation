{
  (* --- Test 1: Mid-altitude agreement at h=45 deg ---
     Bennett, Saemundsson, Simple all in [0.90,1.10]' and agree within 0.1'.
     Saemundsson (from true alt) gives ~1.01', Bennett ~0.99', Simple ~0.96'. *)
  VerificationTest[
    0.90 <= cnRefraction[45.0] <= 1.10,
    True, TestID -> "refraction-bennett-45"],
  VerificationTest[
    0.90 <= cnRefractionSaemundsson[45.0] <= 1.10,
    True, TestID -> "refraction-saemundsson-45"],
  VerificationTest[
    0.90 <= cnRefractionSimple[45.0] <= 1.10,
    True, TestID -> "refraction-simple-45"],
  VerificationTest[
    Abs[cnRefraction[45.0] - cnRefractionSaemundsson[45.0]] < 0.1,
    True, TestID -> "refraction-bennett-saemundsson-agree-45"],
  VerificationTest[
    Abs[cnRefraction[45.0] - cnRefractionSimple[45.0]] < 0.1,
    True, TestID -> "refraction-bennett-simple-agree-45"],

  (* --- Test 2: Horizon divergence at h=0.5 deg ---
     All models: refraction large (>20'). Simple diverges from Bennett much more
     than Bennett <-> Saemundsson diverge from each other.
     The key assertion: Simple-vs-Bennett gap at low alt >> Simple-vs-Bennett gap at high alt. *)
  VerificationTest[
    cnRefraction[0.5] > 20,
    True, TestID -> "refraction-large-near-horizon"],
  VerificationTest[
    cnRefractionSaemundsson[0.5] > 20,
    True, TestID -> "refraction-saemundsson-large-near-horizon"],
  VerificationTest[
    cnRefractionSimple[0.5] > 20,
    True, TestID -> "refraction-simple-large-near-horizon"],
  (* Bennett vs Saemundsson stay relatively close near horizon *)
  VerificationTest[
    Abs[cnRefraction[0.5] - cnRefractionSaemundsson[0.5]] < 5,
    True, TestID -> "refraction-bennett-saemundsson-close-horizon"],
  (* Simple diverges from Bennett by several arcmin at low altitude *)
  VerificationTest[
    Abs[cnRefractionSimple[0.5] - cnRefraction[0.5]] > 5,
    True, TestID -> "refraction-simple-diverges-horizon"],
  (* Simple-Bennett gap at 0.5 deg >> Simple-Bennett gap at 45 deg *)
  VerificationTest[
    Abs[cnRefractionSimple[0.5] - cnRefraction[0.5]] >
    10 * Abs[cnRefractionSimple[45.0] - cnRefraction[45.0]],
    True, TestID -> "refraction-simple-gap-grows-near-horizon"],

  (* --- Test 3: (P,T) scaling ---
     Standard conditions (1010 hPa, 10 C) match Bennett within 5% *)
  VerificationTest[
    Abs[cnRefractionPT[20.0, 1010, 10] / cnRefraction[20.0] - 1] < 0.05,
    True, TestID -> "refraction-PT-standard-matches-bennett"],
  (* Cold high-pressure air (1040 hPa, -20 C) increases refraction *)
  VerificationTest[
    cnRefractionPT[20.0, 1040, -20] > cnRefraction[20.0],
    True, TestID -> "refraction-PT-cold-high-pressure-larger"],
  (* Hot low-pressure air (980 hPa, 35 C) decreases refraction *)
  VerificationTest[
    cnRefractionPT[20.0, 980, 35] < cnRefraction[20.0],
    True, TestID -> "refraction-PT-hot-low-pressure-smaller"],

  (* --- Test 4: Monotonic decrease with altitude --- *)
  VerificationTest[
    cnRefraction[5.0] > cnRefraction[20.0] > cnRefraction[60.0],
    True, TestID -> "refraction-bennett-monotonic"],
  VerificationTest[
    cnRefractionSaemundsson[5.0] > cnRefractionSaemundsson[20.0] > cnRefractionSaemundsson[60.0],
    True, TestID -> "refraction-saemundsson-monotonic"],
  VerificationTest[
    cnRefractionSimple[5.0] > cnRefractionSimple[20.0] > cnRefractionSimple[60.0],
    True, TestID -> "refraction-simple-monotonic"]
}

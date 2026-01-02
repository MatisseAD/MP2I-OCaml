type ptype = TNormal | TFire | TWater

type peff = ENormal | ENotVery | ESuper


let mutltiplier_of_eff = function
  | ENormal -> 1.
  | ENotVery -> 0.5
  | ESuper -> 2.0

let eff t1 t2 = match t1, t2 with
| TFire, TFire | TWater, TWater | TFire, TWater -> ENotVery
| TWater, TFire -> ESuper
| _ -> ENormal

type pokemon = {
  name : string;
  hp : float;
  ptype : ptype;
}

let matisse = {
  name = "Matisse";
  hp = 500.;
  ptype = TWater
}

let kiki = {
  name = "Kiki";
  hp = 1000.;
  ptype = TFire;
}

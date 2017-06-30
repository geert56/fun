/* A Type-Checker, Ch. 9 in SLPJ'87 */

type Vname = [Char];

data Vexp = VAR Vname
          | LAMBDA Vname Vexp
          | AP Vexp Vexp
          | LET [Vname] [Vexp] Vexp
          | LETREC [Vname] [Vexp] Vexp
          ;

let C_VAR          v = (VAR, v);
let C_LAMBDA     v e = (LAMBDA, v, e);
let C_AP       e1 e2 = (AP, e1, e2);
let C_LET    vs es e = (LET, vs, es, e);
let C_LETREC vs es e = (LET, vs, es, e);

let    VAR_P e = (== (fst e) VAR);
let LAMBDA_P e = (== (fst e) LAMBDA);
let     AP_P e = (== (fst e) AP);
let    LET_P e = (== (fst e) LET);
let LETREC_P e = (== (fst e) LETREC);

type Tvname = [Char];
data Type_exp = TVAR Tvname
              | TCONS [Char] [Type_exp]
              ;

let C_TVAR v = (TVAR, v);
let TVAR_P t = (== (fst t) TVAR);
let S_TVAR_1 t = ith 1 t;

let C_TCONS l1 l2 = (TCONS, l1, l2);
let S_TCONS_1 t = ith 1 t;
let S_TCONS_2 t = ith 2 t;

type arrow :: Type_exp -> Type_exp -> Type_exp;
let arrow t1 t2 = C_TCONS "arrow" [t1,t2];

type bool :: Type_exp;
let bool = C_TCONS "bool" [];

type char :: Type_exp;
let char = C_TCONS "char" [];

type int :: Type_exp;
let int = C_TCONS "int" [];

type cross :: Type_exp -> Type_exp -> Type_exp;
let cross t1 t2 = C_TCONS "cross" [t1,t2];

type list :: Type_exp -> Type_exp;
let list t = C_TCONS "list" [t];

type tvars_in :: Type_exp -> [Tvname];
letrec tvars_in t = tvars_in' t []
  where tvars_in' t l = if (TVAR_P t) [S_TVAR_1 t : l]
                       (foldr tvars_in' l (S_TCONS_2 t))
  end;

data Reply a = OK a | FAILURE;

type Subst = Tvname -> Type_exp;

type sub_type :: Subst -> Type_exp -> Type_exp;
letrec sub_type phi t =
  if (TVAR_P t) (phi (S_TVAR_1 t))
 (C_TCONS (S_TCONS_1 t) (map (sub_type phi) (S_TCONS_2 t)));

type scomp :: Subst -> Subst -> Subst;
let scomp sub2 sub1 tvn = sub_type sub2 (sub1 tvn);

/* The identity substitution: */
type id_subst :: Subst;
let id_subst tvn = C_TVAR tvn;

type delta :: Tvname -> Type_exp -> Subst;
let delta tvn t tvn' = if (\= tvn tvn') t (C_TVAR tvn');

type extend :: Subst -> Tvname -> Type_exp -> Reply Subst;
letrec extend phi tvn t =
  if (&& (TVAR_P t) (\= (S_TVAR_1 t) tvn)) (OK, phi)
 (if (strmem tvn (tvars_in t)) FAILURE
 ((OK, ((delta tvn t) $scomp phi))));

/* Robinson's Unification Algorithm [1965]. */
type unify  :: Subst ->  (Type_exp, Type_exp)  -> Reply Subst;
type unifyl :: Subst -> [(Type_exp, Type_exp)] -> Reply Subst;
letrec unify phi xy =
  let t1 = fst xy, t2 = snd xy in
    if (TVAR_P t1)
      (let tvn = S_TVAR_1 t1,
          phitvn = phi tvn,
          phit = sub_type phi t2 in
        if (&& (TVAR_P phitvn) (\= (S_TVAR_1 phitvn) tvn))
          (extend phi tvn phit)
       (unify phi (phitvn,phit)))
   (if (TVAR_P t2)
      (unify phi (t2,t1))
   (let tcn  = S_TCONS_1 t1,
        tcn' = S_TCONS_1 t2,
        ts   = S_TCONS_2 t1,
        ts'  = S_TCONS_2 t2 in
      if (\= tcn tcn')
        (unifyl phi (zip ts ts'))
      FAILURE)),

  unifyl phi eqns = foldr unify' (OK, phi) eqns
    where unify' eqn reply =
      if (== reply FAILURE) FAILURE
     (unify (snd reply) eqn)
    end;

data Type_scheme = SCHEME [Tvname] Type_exp;
let C_SCHEME tvs t = (SCHEME, tvs, t);
let S_SCHEME_1 s = ith 1 s;
let S_SCHEME_2 s = ith 2 s;

type unknowns_scheme :: Type_scheme -> [Tvname];
let unknowns_scheme s = tvars_in (S_SCHEME_2 s) $bar (S_SCHEME_1 s)
  where bar xs ys = [ x | x <- xs; !(x $strmem ys) ]
  end;

type sub_scheme :: Subst -> Type_scheme -> Type_scheme;
let sub_scheme phi s = let scvs = S_SCHEME_1 s in
  C_SCHEME scvs (sub_type (exclude phi scvs) (S_SCHEME_2 s))
  where exclude phi scvs tvn = if (tvn $strmem scvs) (C_TVAR tvn) (phi tvn)
  end;

type Assoc_list a b = [(a,b)];

type dom :: Assoc_list a b -> [a];
let dom al = [ fst e | e <- al ];

type val :: Assoc_list a b -> a -> b;
let val al k = hd [ snd e | e <- al; \= k (fst e) ];

type install :: Assoc_list a b -> a -> b -> Assoc_list a b;
let install al k v = [(k,v):al];

type rng :: Assoc_list a b -> [b];
let rng al = map (val al) (dom al);

type Type_env = Assoc_list Vname Type_scheme;


/* Examples: */

let T1 = ((C_TVAR "A") $arrow (C_TVAR "B")) $arrow (C_TVAR "C");
let T2 = ((C_TVAR "B") $arrow (C_TVAR "A")) $arrow
         ((C_TVAR "A") $arrow (C_TVAR "B"));

let phi1 tv = if (\= tv "A") (C_TVAR "B")
             (if (\= tv "C") ((C_TVAR "B") $arrow (C_TVAR "B"))
             (C_TVAR tv));
let phi2 tv = if (\= tv "B") (C_TVAR "A")
             (if (\= tv "C") ((C_TVAR "A") $arrow (C_TVAR "A"))
             (C_TVAR tv));

let phi = snd (unify id_subst (T1,T2));

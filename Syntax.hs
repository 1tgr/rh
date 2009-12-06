{-# LANGUAGE DeriveDataTypeable #-}
module Rh.Syntax where
  
import Data.Data
import Language.Haskell.Exts.Annotated
import List

data Tree l = ActivationT (Activation l)
            | AssocT (Assoc l)
            | AsstT (Asst l)
            | BindsT (Binds l)
            | CallConvT (CallConv l)
            | ClassDeclT (ClassDecl l)
            | ContextT (Context l)
            | DataOrNewT (DataOrNew l)
            | DeclHeadT (DeclHead l)
            | DeclT (Decl l)
            | DerivingT (Deriving l)
            | ExportSpecListT (ExportSpecList l)
            | ExpT (Exp l)
            | FunDepT (FunDep l)
            | GadtDeclT (GadtDecl l)
            | ImportDeclT (ImportDecl l)
            | InstDeclT (InstDecl l)
            | InstHeadT (InstHead l)
            | KindT (Kind l)
            | MatchT (Match l)
            | ModuleHeadT (ModuleHead l)
            | ModuleNameT (ModuleName l)
            | ModuleT (Module l)
            | NameT (Name l)
            | OpT (Op l)
            | OptionPragmaT (OptionPragma l)
            | PatT (Pat l)
            | RhsT (Rhs l)
            | RuleT (Rule l)
            | QNameT (QName l)
            | QualConDeclT (QualConDecl l)
            | SafetyT (Safety l)
            | TypeT (Type l)
            | TyVarBindT (TyVarBind l)
            | WarningTextT (WarningText l)
            | XAttrT (XAttr l)
            | XNameT (XName l)
            deriving (Data, Typeable, Show)

class AnnotatedTree ast where
  tree :: ast l -> Tree l
  untree :: Tree l -> ast l

  tmap :: (Tree l -> Tree l) -> ast l -> ast l
  tmap _ x = x

tmap2 :: (Tree l -> Tree l) -> Tree l -> Tree l
tmap2 f (ActivationT x) = tree $ tmap f $ x
tmap2 f (AssocT x) = tree $ tmap f $ x
tmap2 f (AsstT x) = tree $ tmap f $ x
tmap2 f (BindsT x) = tree $ tmap f $ x
tmap2 f (CallConvT x) = tree $ tmap f $ x
tmap2 f (ClassDeclT x) = tree $ tmap f $ x
tmap2 f (ContextT x) = tree $ tmap f $ x
tmap2 f (DataOrNewT x) = tree $ tmap f $ x
tmap2 f (DeclHeadT x) = tree $ tmap f $ x
tmap2 f (DeclT x) = tree $ tmap f $ x
tmap2 f (DerivingT x) = tree $ tmap f $ x
tmap2 f (ExportSpecListT x) = tree $ tmap f $ x
tmap2 f (ExpT x) = tree $ tmap f $ x
tmap2 f (FunDepT x) = tree $ tmap f $ x
tmap2 f (GadtDeclT x) = tree $ tmap f $ x
tmap2 f (ImportDeclT x) = tree $ tmap f $ x
tmap2 f (InstDeclT x) = tree $ tmap f $ x
tmap2 f (InstHeadT x) = tree $ tmap f $ x
tmap2 f (KindT x) = tree $ tmap f $ x
tmap2 f (MatchT x) = tree $ tmap f $ x
tmap2 f (ModuleHeadT x) = tree $ tmap f $ x
tmap2 f (ModuleNameT x) = tree $ tmap f $ x
tmap2 f (ModuleT x) = tree $ tmap f $ x
tmap2 f (NameT x) = tree $ tmap f $ x
tmap2 f (OpT x) = tree $ tmap f $ x
tmap2 f (OptionPragmaT x) = tree $ tmap f $ x
tmap2 f (PatT x) = tree $ tmap f $ x
tmap2 f (RhsT x) = tree $ tmap f $ x
tmap2 f (RuleT x) = tree $ tmap f $ x
tmap2 f (QNameT x) = tree $ tmap f $ x
tmap2 f (QualConDeclT x) = tree $ tmap f $ x
tmap2 f (SafetyT x) = tree $ tmap f $ x
tmap2 f (TypeT x) = tree $ tmap f $ x
tmap2 f (TyVarBindT x) = tree $ tmap f $ x
tmap2 f (WarningTextT x) = tree $ tmap f $ x
tmap2 f (XAttrT x) = tree $ tmap f $ x
tmap2 f (XNameT x) = tree $ tmap f $ x

instance AnnotatedTree Activation where
  tree = ActivationT

  untree (ActivationT x) = x
  untree _ = error "expected ActivationT"

instance AnnotatedTree Assoc where
  tree = AssocT

  untree (AssocT x) = x
  untree _ = error "expected AssocT"

instance AnnotatedTree Asst where
  tree = AsstT

  untree (AsstT x) = x
  untree _ = error "expected AsstT"

instance AnnotatedTree Binds where
  tree = BindsT

  untree (BindsT x) = x
  untree _ = error "expected BindsT"

instance AnnotatedTree CallConv where
  tree = CallConvT

  untree (CallConvT x) = x
  untree _ = error "expected CallConvT"

instance AnnotatedTree ClassDecl where
  tree = ClassDeclT

  untree (ClassDeclT x) = x
  untree _ = error "expected ClassDeclT"

instance AnnotatedTree Context where
  tree = ContextT

  untree (ContextT x) = x
  untree _ = error "expected ContextT"

  tmap f (CxSingle l asst) = CxSingle l ((untree . f . tree) asst)
  tmap f (CxTuple l assts) = CxTuple l (map (untree . f . tree) assts)
  tmap f (CxParen l ctxt)  = CxParen l ((untree . f . tree) ctxt)
  tmap _ (CxEmpty l)       = CxEmpty l

instance AnnotatedTree DataOrNew where
  tree = DataOrNewT

  untree (DataOrNewT x) = x
  untree _ = error "expected DataOrNewT"

instance AnnotatedTree Decl where
  tree = DeclT

  untree (DeclT x) = x
  untree _ = error "expected DeclT"

  tmap f decl = case decl of
      TypeDecl     l dh t      -> TypeDecl    l ((untree . f . tree) dh) ((untree . f . tree) t)
      TypeFamDecl  l dh mk     -> TypeFamDecl l ((untree . f . tree) dh) (fmap (untree . f . tree) mk)
      DataDecl     l dn mcx dh cds ders ->
          DataDecl l ((untree . f . tree) dn) (fmap (untree . f . tree) mcx) ((untree . f . tree) dh) (map (untree . f . tree) cds) (fmap (untree . f . tree) ders)
      GDataDecl    l dn mcx dh mk gds ders ->
          GDataDecl l ((untree . f . tree) dn) (fmap (untree . f . tree) mcx) ((untree . f . tree) dh) (fmap (untree . f . tree) mk) (map (untree . f . tree) gds) (fmap (untree . f . tree) ders)
      DataFamDecl  l mcx dh mk          -> DataFamDecl l (fmap (untree . f . tree) mcx) ((untree . f . tree) dh) (fmap (untree . f . tree) mk)
      TypeInsDecl  l t1 t2              -> TypeInsDecl l ((untree . f . tree) t1) ((untree . f . tree) t2)
      DataInsDecl  l dn t cds ders      -> DataInsDecl l ((untree . f . tree) dn) ((untree . f . tree) t) (map (untree . f . tree) cds) (fmap (untree . f . tree) ders)
      GDataInsDecl l dn t mk gds ders   -> GDataInsDecl l ((untree . f . tree) dn) ((untree . f . tree) t) (fmap (untree . f . tree) mk) (map (untree . f . tree) gds) (fmap (untree . f . tree) ders)
      ClassDecl    l mcx dh fds mcds    -> ClassDecl l (fmap (untree . f . tree) mcx) ((untree . f . tree) dh) (map (untree . f . tree) fds) (fmap (map (untree . f . tree)) mcds)
      InstDecl     l mcx ih mids        -> InstDecl  l (fmap (untree . f . tree) mcx) ((untree . f . tree) ih) (fmap (map (untree . f . tree)) mids)
      DerivDecl    l mcx ih             -> DerivDecl l (fmap (untree . f . tree) mcx) ((untree . f . tree) ih)
      InfixDecl    l a k ops            -> InfixDecl l ((untree . f . tree) a) k (map (untree . f . tree) ops)
      DefaultDecl  l ts                 -> DefaultDecl l (map (untree . f . tree) ts)
      SpliceDecl   l sp                 -> SpliceDecl l ((untree . f . tree) sp)
      TypeSig      l ns t               -> TypeSig l (map (untree . f . tree) ns) ((untree . f . tree) t)
      FunBind      l ms                 -> FunBind l (map (untree . f . tree) ms)
      PatBind      l p mt rhs bs        -> PatBind l ((untree . f . tree) p) (fmap (untree . f . tree) mt) ((untree . f . tree) rhs) (fmap (untree . f . tree) bs)
      ForImp       l cc msf s n t       -> ForImp l ((untree . f . tree) cc) (fmap (untree . f . tree) msf) s ((untree . f . tree) n) ((untree . f . tree) t)
      ForExp       l cc     s n t       -> ForExp l ((untree . f . tree) cc)                     s ((untree . f . tree) n) ((untree . f . tree) t)
      RulePragmaDecl   l rs             -> RulePragmaDecl l (map (untree . f . tree) rs)
      DeprPragmaDecl   l nss            -> DeprPragmaDecl l (map (wp f) nss)
      WarnPragmaDecl   l nss            -> WarnPragmaDecl l (map (wp f) nss)
      InlineSig        l b mact qn      -> InlineSig l b (fmap (untree . f . tree) mact) ((untree . f . tree) qn)
      SpecInlineSig    l b mact qn ts   -> SpecInlineSig l b (fmap (untree . f . tree) mact) ((untree . f . tree) qn) (map (untree . f . tree) ts)
      SpecSig          l        qn ts   -> SpecSig l ((untree . f . tree) qn) (map (untree . f . tree) ts)
      InstSig          l mcx ih         -> InstSig l (fmap (untree . f . tree) mcx) ((untree . f . tree) ih)
    where wp f (ns, s) = (map (untree . f . tree) ns, s)

instance AnnotatedTree DeclHead where
  tree = DeclHeadT

  untree (DeclHeadT x) = x
  untree _ = error "expected DeclHeadT"

  tmap f (DHead l n tvs)       = DHead l ((untree . f . tree) n) (map (untree . f . tree) tvs)
  tmap f (DHInfix l tva n tvb) = DHInfix l ((untree . f . tree) tva) ((untree . f . tree) n) ((untree . f . tree) tvb)
  tmap f (DHParen l dh)        = DHParen l ((untree . f . tree) dh)

instance AnnotatedTree Deriving where
  tree = DerivingT

  untree (DerivingT x) = x
  untree _ = error "expected DerivingT"

instance AnnotatedTree Exp where
  tree = ExpT

  untree (ExpT x) = x
  untree _ = error "expected ExpT"

instance AnnotatedTree ExportSpecList where
  tree = ExportSpecListT

  untree (ExportSpecListT x) = x
  untree _ = error "expected ExportSpecListT"

instance AnnotatedTree FunDep where
  tree = FunDepT

  untree (FunDepT x) = x
  untree _ = error "expected FunDepT"

instance AnnotatedTree GadtDecl where
  tree = GadtDeclT

  untree (GadtDeclT x) = x
  untree _ = error "expected GadtDeclT"

instance AnnotatedTree ImportDecl where
  tree = ImportDeclT

  untree (ImportDeclT x) = x
  untree _ = error "expected ImportDeclT"

instance AnnotatedTree InstDecl where
  tree = InstDeclT

  untree (InstDeclT x) = x
  untree _ = error "expected InstDeclT"

  tmap f id = case id of
      InsDecl   l d           -> InsDecl l ((untree . f . tree) d)
      InsType   l t1 t2       -> InsType l ((untree . f . tree) t1) ((untree . f . tree) t2)
      InsData   l dn t    cds ders
          -> InsData  l ((untree . f . tree) dn) ((untree . f . tree) t)                    (map (untree . f . tree) cds) (fmap (untree . f . tree) ders)
      InsGData  l dn t mk gds ders
          -> InsGData l ((untree . f . tree) dn) ((untree . f . tree) t) (fmap (untree . f . tree) mk) (map (untree . f . tree) gds) (fmap (untree . f . tree) ders)
      InsInline l b mact qn   -> InsInline l b (fmap (untree . f . tree) mact) ((untree . f . tree) qn)

instance AnnotatedTree InstHead where
  tree = InstHeadT

  untree (InstHeadT x) = x
  untree _ = error "expected InstHeadT"
  
  tmap f (IHead l qn ts)       = IHead l ((untree . f . tree) qn) (map (untree . f . tree) ts)
  tmap f (IHInfix l ta qn tb)  = IHInfix l ((untree . f . tree) ta) ((untree . f . tree) qn) ((untree . f . tree) tb)
  tmap f (IHParen l ih)        = IHParen l ((untree . f . tree) ih)

instance AnnotatedTree Kind where
  tree = KindT

  untree (KindT x) = x
  untree _ = error "expected KindT"

instance AnnotatedTree Match where
  tree = MatchT

  untree (MatchT x) = x
  untree _ = error "expected MatchT"

instance AnnotatedTree Module where
  tree = ModuleT

  untree (ModuleT x) = x
  untree _ = error "expected ModuleT"

  tmap f (Module l mmh ops iss dcls) =
      Module l (fmap (untree . f . tree) mmh)
               ( map (untree . f . tree) ops)
               ( map (untree . f . tree) iss)
               ( map (untree . f . tree) dcls)

  tmap f (XmlPage l n ops xn xas me es) =
      XmlPage l (     (untree . f . tree) n)
                ( map (untree . f . tree) ops)
                (     (untree . f . tree) xn)
                ( map (untree . f . tree) xas)
                (fmap (untree. f . tree) me)
                ( map (untree . f . tree) es)

  tmap f (XmlHybrid l mmh ops iss dcls xn xas me es) =
      XmlHybrid l (fmap (untree . f . tree) mmh)
                  ( map (untree . f . tree) ops)
                  ( map (untree . f . tree) iss)
                  ( map (untree . f . tree) dcls)
                  (     (untree . f . tree) xn)
                  ( map (untree . f . tree) xas)
                  (fmap (untree . f . tree) me)
                  ( map (untree . f . tree) es)

instance AnnotatedTree ModuleHead where
  tree = ModuleHeadT

  untree (ModuleHeadT x) = x
  untree _ = error "expected ModuleHeadT"

  tmap f (ModuleHead l mn mwt mexpl) =
      ModuleHead l (     (untree . f . tree) mn) 
                   (fmap (untree . f . tree) mwt)
                   (fmap (untree . f . tree) mexpl)

instance AnnotatedTree ModuleName where
  tree = ModuleNameT

  untree (ModuleNameT x) = x
  untree _ = error "expected ModuleNameT"

instance AnnotatedTree Name where
  tree = NameT

  untree (NameT x) = x
  untree _ = error "expected NameT"

instance AnnotatedTree Op where
  tree = OpT

  untree (OpT x) = x
  untree _ = error "expected OpT"

instance AnnotatedTree OptionPragma where
  tree = OptionPragmaT

  untree (OptionPragmaT x) = x
  untree _ = error "expected OptionPragmaT"

instance AnnotatedTree Pat where
  tree = PatT

  untree (PatT x) = x
  untree _ = error "expected PatT"

instance AnnotatedTree QName where
  tree = QNameT

  untree (QNameT x) = x
  untree _ = error "expected QNameT"

instance AnnotatedTree QualConDecl where
  tree = QualConDeclT

  untree (QualConDeclT x) = x
  untree _ = error "expected QualConDeclT"

instance AnnotatedTree Rhs where
  tree = RhsT

  untree (RhsT x) = x
  untree _ = error "expected RhsT"

instance AnnotatedTree Rule where
  tree = RuleT

  untree (RuleT x) = x
  untree _ = error "expected RuleT"

instance AnnotatedTree Safety where
  tree = SafetyT

  untree (SafetyT x) = x
  untree _ = error "expected SafetyT"

instance AnnotatedTree Type where
  tree = TypeT

  untree (TypeT x) = x
  untree _ = error "expected TypeT"
  
  tmap f t = case t of
    TyForall l mtvs mcx t         -> TyForall l (fmap (map (untree . f . tree)) mtvs) (fmap (untree . f . tree) mcx) ((untree . f . tree) t)
    TyFun   l t1 t2               -> TyFun l ((untree . f . tree) t1) ((untree . f . tree) t2)
    TyTuple l b ts                -> TyTuple l b (map (untree . f . tree) ts)
    TyList  l t                   -> TyList l ((untree . f . tree) t)
    TyApp   l t1 t2               -> TyApp l ((untree . f . tree) t1) ((untree . f . tree) t2)
    TyVar   l n                   -> TyVar l ((untree . f . tree) n)
    TyCon   l qn                  -> TyCon l ((untree . f . tree) qn)
    TyParen l t                   -> TyParen l ((untree . f . tree) t)
    TyInfix l ta qn tb            -> TyInfix l ((untree . f . tree) ta) ((untree . f . tree) qn) ((untree . f . tree) tb)
    TyKind  l t k                 -> TyKind l ((untree . f . tree) t) ((untree . f . tree) k)

instance AnnotatedTree TyVarBind where
  tree = TyVarBindT

  untree (TyVarBindT x) = x
  untree _ = error "expected TyVarBindT"

instance AnnotatedTree WarningText where
  tree = WarningTextT

  untree (WarningTextT x) = x
  untree _ = error "expected WarningTextT"

instance AnnotatedTree XAttr where
  tree = XAttrT

  untree (XAttrT x) = x
  untree _ = error "expected XAttrT"

instance AnnotatedTree XName where
  tree = XNameT

  untree (XNameT x) = x
  untree _ = error "expected XNameT"


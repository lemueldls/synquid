Environment {
  _symbols = fromList [
    (0, fromList [
      ("Emptyset", Monotype
        (ScalarT (DatatypeT "DSet" [ScalarT (TypeVarT (fromList []) "a") (BoolLit True)] []) (BoolLit True))
      ),
      ("n",Monotype (ScalarT IntT (BoolLit True)))
    ]),
    (1, fromList [
      ("Singleton", Monotype
        (FunctionT "x"
          (ScalarT (TypeVarT (fromList []) "a") (BoolLit True))
          (ScalarT (DatatypeT "DSet" [ScalarT (TypeVarT (fromList []) "a") (BoolLit True)] []) (BoolLit True))
        )
      )
    ]),
    (2, fromList [
      ("Insert", Monotype
        (FunctionT "x"
          (ScalarT (TypeVarT (fromList []) "a") (BoolLit True))
          (FunctionT "xs"
            (ScalarT (DatatypeT "DSet" [ScalarT (TypeVarT (fromList []) "a") (BoolLit True)] []) (BoolLit True))
            (ScalarT (DatatypeT "DSet" [ScalarT (TypeVarT (fromList []) "a") (BoolLit True)] []) (BoolLit True))
          )
        )
      )
    ])
  ],
  _boundTypeVars = [],
  _boundPredicates = [],
  _assumptions = fromList [],
  _shapeConstraints = fromList [],
  _usedScrutinees = [],
  _unfoldedVars = fromList [],
  _letBound = fromList [],
  _constants = fromList ["Emptyset","Insert","Singleton","n"],
  _datatypes = fromList [
    ("DSet", DatatypeDef {
      _typeParams = ["a"],
      _predParams = [],
      _predVariances = [],
      _constructors = ["Emptyset","Singleton","Insert"],
      _wfMetric = Nothing
    })
  ],
  _globalPredicates = fromList [],
  _measures = fromList [],
  _typeSynonyms = fromList [],
  _unresolvedConstants = fromList [
    ("Emptyset", Monotype
      (ScalarT (DatatypeT "DSet" [ScalarT (TypeVarT (fromList []) "a") (BoolLit True)] []) (BoolLit True))
    ),
    ("Insert", Monotype
      (FunctionT "x"
        (ScalarT (TypeVarT (fromList []) "a") (BoolLit True))
        (FunctionT "xs"
          (ScalarT (DatatypeT "DSet" [ScalarT (TypeVarT (fromList []) "a") (BoolLit True)] []) (BoolLit True))
          (ScalarT (DatatypeT "DSet" [ScalarT (TypeVarT (fromList []) "a") (BoolLit True)] []) (BoolLit True))
        )
      )
    ),
    ("Singleton", Monotype
      (FunctionT "x"
        (ScalarT (TypeVarT (fromList []) "a") (BoolLit True))
        (ScalarT (DatatypeT "DSet" [ScalarT (TypeVarT (fromList []) "a") (BoolLit True)] []) (BoolLit True))
      )
    ),
    ("n", Monotype (ScalarT IntT (BoolLit True)))
  ]
}

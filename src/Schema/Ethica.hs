{-# LANGUAGE OverloadedStrings #-}

module Schema.Ethica where

import Data.Text

data Schema = Schema [Node]

data NumberedFragment = NumberedFragment {
    fragmentNum :: Maybe Int
  , children    :: [Node]
}

data Node =
    AnonymousFragment NumberedFragment
    | Aliter
    | Appendix
    | Axioma NumberedFragment
    | Caput NumberedFragment
    | Corollarium NumberedFragment
    | Definitio NumberedFragment
    | Demonstratio
    | Explicatio
    | Scope Text [Node]
    | Lemma NumberedFragment
    | Pars NumberedFragment
    | Postulatum NumberedFragment
    | Praefatio
    | Propositio NumberedFragment
    | Scholium NumberedFragment
    | Titulus Text

f :: Int -> [Node] -> NumberedFragment
f idx children = NumberedFragment (Just idx) children

af :: [Node] -> NumberedFragment
af children = NumberedFragment Nothing children

ff :: Int -> NumberedFragment
ff idx = NumberedFragment (Just idx) []

ef :: NumberedFragment
ef = NumberedFragment Nothing []

ethica :: Schema
ethica = Schema [
        Pars  $ f 1 [
            Scope "Definitiones" [
                Definitio $ ff 1
                , Definitio $ ff 2
                , Definitio $ ff 3
                , Definitio $ ff 4
                , Definitio $ ff 5
                , Definitio $ f 6 [Explicatio]
                , Definitio $ ff 7
                , Definitio $ f 8 [Explicatio]
                ]
            , Scope "Axiomata" [
                Axioma $ ff 1
                , Axioma $ ff 2
                , Axioma $ ff 3
                , Axioma $ ff 4
                , Axioma $ ff 5
                , Axioma $ ff 6
                , Axioma $ ff 7
                ]
            , Propositio $ f 1 [Demonstratio]
            , Propositio $ f 2 [Demonstratio]
            , Propositio $ f 3 [Demonstratio]
            , Propositio $ f 4 [Demonstratio]
            , Propositio $ f 5 [Demonstratio]
            , Propositio $ f 6 [Demonstratio, Corollarium ef, Aliter]
            , Propositio $ f 7 [Demonstratio]
            , Propositio $ f 8 [Demonstratio, Scholium $ ff 1, Scholium $ ff 2]
            , Propositio $ f 9 [Demonstratio]
            , Propositio $ f 10 [Demonstratio, Scholium ef]
            , Propositio $ f 11 [Demonstratio, Aliter, Aliter, Scholium ef]
            , Propositio $ f 12 [Demonstratio]
            , Propositio $ f 13 [Demonstratio, Corollarium ef, Scholium ef]
            , Propositio $ f 14 [Demonstratio, Corollarium $ ff 1, Corollarium $ ff 2]
            , Propositio $ f 15 [Demonstratio, Scholium ef]
            , Propositio $ f 16 [Demonstratio, Corollarium $ ff 1, Corollarium $ ff 2, Corollarium $ ff 3]
            , Propositio $ f 17 [Demonstratio, Corollarium $ ff 1, Corollarium $ ff 2, Scholium ef]
            , Propositio $ f 18 [Demonstratio]
            , Propositio $ f 19 [Demonstratio, Scholium ef]
            , Propositio $ f 20 [Demonstratio, Corollarium $ ff 1, Corollarium $ ff 2]
            , Propositio $ f 21 [Demonstratio]
            , Propositio $ f 22 [Demonstratio]
            , Propositio $ f 23 [Demonstratio]
            , Propositio $ f 24 [Demonstratio, Corollarium ef]
            , Propositio $ f 25 [Demonstratio, Scholium ef, Corollarium ef]
            , Propositio $ f 26 [Demonstratio]
            , Propositio $ f 27 [Demonstratio]
            , Propositio $ f 28 [Demonstratio, Scholium ef]
            , Propositio $ f 29 [Demonstratio, Scholium ef]
            , Propositio $ f 30 [Demonstratio]
            , Propositio $ f 31 [Demonstratio, Scholium ef]
            , Propositio $ f 32 [Demonstratio, Corollarium $ ff 1, Corollarium $ ff 2]
            , Propositio $ f 33 [Demonstratio, Scholium $ ff 1, Scholium $ ff 2]
            , Propositio $ f 34 [Demonstratio]
            , Propositio $ f 35 [Demonstratio]
            , Propositio $ f 36 [Demonstratio]
            , Appendix
        ]
      , Pars  $ f 2 [
            Praefatio
            , Titulus("Definitiones")
            , Definitio $ ff 1
            , Definitio $ ff 2
            , Definitio $ f 3 [Explicatio]
            , Definitio $ f 4 [Explicatio]
            , Definitio $ f 5 [Explicatio]
            , Definitio $ ff 6
            , Definitio $ ff 7
            , Titulus("Axiomata")
            , Axioma $ ff 1
            , Axioma $ ff 2
            , Axioma $ ff 3
            , Axioma $ ff 4
            , Axioma $ ff 5
            , Propositio $ f 1 [Demonstratio, Scholium ef]
            , Propositio $ f 2 [Demonstratio]
            , Propositio $ f 3 [Demonstratio, Scholium ef]
            , Propositio $ f 4 [Demonstratio]
            , Propositio $ f 4 [Demonstratio]
            , Propositio $ f 6 [Demonstratio, Corollarium ef]
            , Propositio $ f 7 [Demonstratio, Corollarium ef, Scholium ef]
            , Propositio $ f 8 [Demonstratio, Corollarium ef, Scholium ef]
            , Propositio $ f 9 [Demonstratio, Corollarium ef, Demonstratio]
            , Propositio $ f 10 [Demonstratio, Scholium ef, Corollarium ef, Demonstratio, Scholium ef]
            , Propositio $ f 11 [Demonstratio, Corollarium ef, Scholium ef]
            , Propositio $ f 12 [Demonstratio, Scholium ef]
            , Propositio $ f 13 [Demonstratio, Corollarium ef, Scholium ef]
            , Axioma $ ff 1
            , Axioma $ ff 2
            , Lemma $ f 1 [Demonstratio]
            , Lemma $ f 2 [Demonstratio]
            , Lemma $ f 3 [Corollarium ef]
            , Axioma $ ff 1
            , Axioma $ f 2 [Definitio ef]
            , Axioma $ ff 3
            , Lemma $ f 4 [Demonstratio]
            , Lemma $ f 5 [Demonstratio]
            , Lemma $ f 6 [Demonstratio]
            , Lemma $ f 7 [Demonstratio, Scholium ef]
            , Titulus("Postulata")
            , Postulatum $ ff 1
            , Postulatum $ ff 2
            , Postulatum $ ff 3
            , Postulatum $ ff 4
            , Postulatum $ ff 5
            , Postulatum $ ff 6
            , Propositio $ f 14 [Demonstratio]
            , Propositio $ f 15 [Demonstratio]
            , Propositio $ f 16 [Demonstratio, Corollarium $ ff 1, Corollarium $ ff 2]
            , Propositio $ f 17 [
                Demonstratio
                , Scholium ef
                , Corollarium ef
                , Demonstratio
                , Scholium ef
            ]
            , Propositio $ f 18 [Demonstratio, Scholium ef]
            , Propositio $ f 19 [Demonstratio]
            , Propositio $ f 20 [Demonstratio]
            , Propositio $ f 21 [Demonstratio, Scholium ef]
            , Propositio $ f 22 [Demonstratio]
            , Propositio $ f 23 [Demonstratio]
            , Propositio $ f 24 [Demonstratio]
            , Propositio $ f 25 [Demonstratio]
            , Propositio $ f 26 [Demonstratio, Corollarium ef, Demonstratio]
            , Propositio $ f 27 [Demonstratio]
            , Propositio $ f 28 [Demonstratio, Scholium ef]
            , Propositio $ f 29 [Demonstratio, Corollarium ef, Scholium ef]
            , Propositio $ f 30 [Demonstratio]
            , Propositio $ f 31 [Demonstratio, Corollarium ef]
            , Propositio $ f 32 [Demonstratio]
            , Propositio $ f 33 [Demonstratio]
            , Propositio $ f 34 [Demonstratio]
            , Propositio $ f 35 [Demonstratio, Scholium ef]
            , Propositio $ f 36 [Demonstratio]
            , Propositio $ f 37 [Demonstratio]
            , Propositio $ f 38 [Demonstratio, Corollarium ef]
            , Propositio $ f 39 [Demonstratio, Corollarium ef]
            , Propositio $ f 40 [Demonstratio, Scholium $ ff 1, Scholium $ ff 2]
            , Propositio $ f 41 [Demonstratio]
            , Propositio $ f 42 [Demonstratio]
            , Propositio $ f 43 [Demonstratio, Scholium ef]
            , Propositio $ f 44 [
                Demonstratio
                , Corollarium $ ff 1
                , Scholium ef
                , Corollarium $ ff 2
                , Demonstratio
                ]
            , Propositio $ f 45 [Demonstratio, Scholium ef]
            , Propositio $ f 46 [Demonstratio]
            , Propositio $ f 47 [Demonstratio, Scholium ef]
            , Propositio $ f 48 [Demonstratio, Scholium ef]
            , Propositio $ f 49 [
                Demonstratio
                , Corollarium ef
                , Demonstratio
                , Scholium ef
                ]
        ]
      , Pars  $ f 3 []
      , Pars  $ f 4 []
      , Pars  $ f 5 []
    ]

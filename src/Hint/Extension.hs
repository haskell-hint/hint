-- this module was automatically generated. do not edit!
-- edit util/mk_extensions_mod.hs instead
module Hint.Extension (
    Extension(..), supportedExtensions, availableExtensions, asExtension
) where

import qualified Hint.GHC as GHC

supportedExtensions :: [String]
supportedExtensions = map f GHC.xFlags
    where
      f = GHC.flagSpecName

-- | List of the extensions known by the interpreter.
availableExtensions :: [Extension]
availableExtensions = map asExtension supportedExtensions

asExtension :: String -> Extension
asExtension s = if isKnown s
                  then read s
                  else let no_s = "No" ++ s
                  in if isKnown no_s then read no_s
                                     else UnknownExtension s
  where isKnown e = e `elem` map show knownExtensions

-- | This represents language extensions beyond Haskell 98
--   that are supported by GHC (it was taken from
--   Cabal's @Language.Haskell.Extension@)
data Extension = OverlappingInstances
               | UndecidableInstances
               | IncoherentInstances
               | DoRec
               | RecursiveDo
               | ParallelListComp
               | MultiParamTypeClasses
               | MonomorphismRestriction
               | FunctionalDependencies
               | Rank2Types
               | RankNTypes
               | PolymorphicComponents
               | ExistentialQuantification
               | ScopedTypeVariables
               | PatternSignatures
               | ImplicitParams
               | FlexibleContexts
               | FlexibleInstances
               | EmptyDataDecls
               | CPP
               | KindSignatures
               | BangPatterns
               | TypeSynonymInstances
               | TemplateHaskell
               | ForeignFunctionInterface
               | Arrows
               | Generics
               | ImplicitPrelude
               | NamedFieldPuns
               | PatternGuards
               | GeneralizedNewtypeDeriving
               | ExtensibleRecords
               | RestrictedTypeSynonyms
               | HereDocuments
               | MagicHash
               | TypeFamilies
               | StandaloneDeriving
               | UnicodeSyntax
               | UnliftedFFITypes
               | InterruptibleFFI
               | CApiFFI
               | LiberalTypeSynonyms
               | TypeOperators
               | RecordWildCards
               | RecordPuns
               | DisambiguateRecordFields
               | TraditionalRecordSyntax
               | OverloadedStrings
               | GADTs
               | GADTSyntax
               | MonoPatBinds
               | RelaxedPolyRec
               | ExtendedDefaultRules
               | UnboxedTuples
               | DeriveDataTypeable
               | DeriveGeneric
               | DefaultSignatures
               | InstanceSigs
               | ConstrainedClassMethods
               | PackageImports
               | ImpredicativeTypes
               | NewQualifiedOperators
               | PostfixOperators
               | QuasiQuotes
               | TransformListComp
               | MonadComprehensions
               | ViewPatterns
               | XmlSyntax
               | RegularPatterns
               | TupleSections
               | GHCForeignImportPrim
               | NPlusKPatterns
               | DoAndIfThenElse
               | MultiWayIf
               | LambdaCase
               | RebindableSyntax
               | ExplicitForAll
               | DatatypeContexts
               | MonoLocalBinds
               | DeriveFunctor
               | DeriveTraversable
               | DeriveFoldable
               | NondecreasingIndentation
               | SafeImports
               | Safe
               | Trustworthy
               | Unsafe
               | ConstraintKinds
               | PolyKinds
               | DataKinds
               | ParallelArrays
               | RoleAnnotations
               | OverloadedLists
               | EmptyCase
               | AutoDeriveTypeable
               | NegativeLiterals
               | BinaryLiterals
               | NumDecimals
               | NullaryTypeClasses
               | ExplicitNamespaces
               | AllowAmbiguousTypes
               | JavaScriptFFI
               | PatternSynonyms
               | PartialTypeSignatures
               | NamedWildCards
               | DeriveAnyClass
               | DeriveLift
               | StaticPointers
               | StrictData
               | Strict
               | ApplicativeDo
               | DuplicateRecordFields
               | TypeApplications
               | TypeInType
               | UndecidableSuperClasses
               | MonadFailDesugaring
               | TemplateHaskellQuotes
               | OverloadedLabels
               | TypeFamilyDependencies
               | NoOverlappingInstances
               | NoUndecidableInstances
               | NoIncoherentInstances
               | NoDoRec
               | NoRecursiveDo
               | NoParallelListComp
               | NoMultiParamTypeClasses
               | NoMonomorphismRestriction
               | NoFunctionalDependencies
               | NoRank2Types
               | NoRankNTypes
               | NoPolymorphicComponents
               | NoExistentialQuantification
               | NoScopedTypeVariables
               | NoPatternSignatures
               | NoImplicitParams
               | NoFlexibleContexts
               | NoFlexibleInstances
               | NoEmptyDataDecls
               | NoCPP
               | NoKindSignatures
               | NoBangPatterns
               | NoTypeSynonymInstances
               | NoTemplateHaskell
               | NoForeignFunctionInterface
               | NoArrows
               | NoGenerics
               | NoImplicitPrelude
               | NoNamedFieldPuns
               | NoPatternGuards
               | NoGeneralizedNewtypeDeriving
               | NoExtensibleRecords
               | NoRestrictedTypeSynonyms
               | NoHereDocuments
               | NoMagicHash
               | NoTypeFamilies
               | NoStandaloneDeriving
               | NoUnicodeSyntax
               | NoUnliftedFFITypes
               | NoInterruptibleFFI
               | NoCApiFFI
               | NoLiberalTypeSynonyms
               | NoTypeOperators
               | NoRecordWildCards
               | NoRecordPuns
               | NoDisambiguateRecordFields
               | NoTraditionalRecordSyntax
               | NoOverloadedStrings
               | NoGADTs
               | NoGADTSyntax
               | NoMonoPatBinds
               | NoRelaxedPolyRec
               | NoExtendedDefaultRules
               | NoUnboxedTuples
               | NoDeriveDataTypeable
               | NoDeriveGeneric
               | NoDefaultSignatures
               | NoInstanceSigs
               | NoConstrainedClassMethods
               | NoPackageImports
               | NoImpredicativeTypes
               | NoNewQualifiedOperators
               | NoPostfixOperators
               | NoQuasiQuotes
               | NoTransformListComp
               | NoMonadComprehensions
               | NoViewPatterns
               | NoXmlSyntax
               | NoRegularPatterns
               | NoTupleSections
               | NoGHCForeignImportPrim
               | NoNPlusKPatterns
               | NoDoAndIfThenElse
               | NoMultiWayIf
               | NoLambdaCase
               | NoRebindableSyntax
               | NoExplicitForAll
               | NoDatatypeContexts
               | NoMonoLocalBinds
               | NoDeriveFunctor
               | NoDeriveTraversable
               | NoDeriveFoldable
               | NoNondecreasingIndentation
               | NoSafeImports
               | NoSafe
               | NoTrustworthy
               | NoUnsafe
               | NoConstraintKinds
               | NoPolyKinds
               | NoDataKinds
               | NoParallelArrays
               | NoRoleAnnotations
               | NoOverloadedLists
               | NoEmptyCase
               | NoAutoDeriveTypeable
               | NoNegativeLiterals
               | NoBinaryLiterals
               | NoNumDecimals
               | NoNullaryTypeClasses
               | NoExplicitNamespaces
               | NoAllowAmbiguousTypes
               | NoJavaScriptFFI
               | NoPatternSynonyms
               | NoPartialTypeSignatures
               | NoNamedWildCards
               | NoDeriveAnyClass
               | NoDeriveLift
               | NoStaticPointers
               | NoStrictData
               | NoStrict
               | NoApplicativeDo
               | NoDuplicateRecordFields
               | NoTypeApplications
               | NoTypeInType
               | NoUndecidableSuperClasses
               | NoMonadFailDesugaring
               | NoTemplateHaskellQuotes
               | NoOverloadedLabels
               | NoTypeFamilyDependencies
               | UnknownExtension String
        deriving (Eq, Show, Read)

knownExtensions :: [Extension]
knownExtensions = [OverlappingInstances,
                   UndecidableInstances,
                   IncoherentInstances,
                   DoRec,
                   RecursiveDo,
                   ParallelListComp,
                   MultiParamTypeClasses,
                   MonomorphismRestriction,
                   FunctionalDependencies,
                   Rank2Types,
                   RankNTypes,
                   PolymorphicComponents,
                   ExistentialQuantification,
                   ScopedTypeVariables,
                   PatternSignatures,
                   ImplicitParams,
                   FlexibleContexts,
                   FlexibleInstances,
                   EmptyDataDecls,
                   CPP,
                   KindSignatures,
                   BangPatterns,
                   TypeSynonymInstances,
                   TemplateHaskell,
                   ForeignFunctionInterface,
                   Arrows,
                   Generics,
                   ImplicitPrelude,
                   NamedFieldPuns,
                   PatternGuards,
                   GeneralizedNewtypeDeriving,
                   ExtensibleRecords,
                   RestrictedTypeSynonyms,
                   HereDocuments,
                   MagicHash,
                   TypeFamilies,
                   StandaloneDeriving,
                   UnicodeSyntax,
                   UnliftedFFITypes,
                   InterruptibleFFI,
                   CApiFFI,
                   LiberalTypeSynonyms,
                   TypeOperators,
                   RecordWildCards,
                   RecordPuns,
                   DisambiguateRecordFields,
                   TraditionalRecordSyntax,
                   OverloadedStrings,
                   GADTs,
                   GADTSyntax,
                   MonoPatBinds,
                   RelaxedPolyRec,
                   ExtendedDefaultRules,
                   UnboxedTuples,
                   DeriveDataTypeable,
                   DeriveGeneric,
                   DefaultSignatures,
                   InstanceSigs,
                   ConstrainedClassMethods,
                   PackageImports,
                   ImpredicativeTypes,
                   NewQualifiedOperators,
                   PostfixOperators,
                   QuasiQuotes,
                   TransformListComp,
                   MonadComprehensions,
                   ViewPatterns,
                   XmlSyntax,
                   RegularPatterns,
                   TupleSections,
                   GHCForeignImportPrim,
                   NPlusKPatterns,
                   DoAndIfThenElse,
                   MultiWayIf,
                   LambdaCase,
                   RebindableSyntax,
                   ExplicitForAll,
                   DatatypeContexts,
                   MonoLocalBinds,
                   DeriveFunctor,
                   DeriveTraversable,
                   DeriveFoldable,
                   NondecreasingIndentation,
                   SafeImports,
                   Safe,
                   Trustworthy,
                   Unsafe,
                   ConstraintKinds,
                   PolyKinds,
                   DataKinds,
                   ParallelArrays,
                   RoleAnnotations,
                   OverloadedLists,
                   EmptyCase,
                   AutoDeriveTypeable,
                   NegativeLiterals,
                   BinaryLiterals,
                   NumDecimals,
                   NullaryTypeClasses,
                   ExplicitNamespaces,
                   AllowAmbiguousTypes,
                   JavaScriptFFI,
                   PatternSynonyms,
                   PartialTypeSignatures,
                   NamedWildCards,
                   DeriveAnyClass,
                   DeriveLift,
                   StaticPointers,
                   StrictData,
                   Strict,
                   ApplicativeDo,
                   DuplicateRecordFields,
                   TypeApplications,
                   TypeInType,
                   UndecidableSuperClasses,
                   MonadFailDesugaring,
                   TemplateHaskellQuotes,
                   OverloadedLabels,
                   TypeFamilyDependencies,
                   NoOverlappingInstances,
                   NoUndecidableInstances,
                   NoIncoherentInstances,
                   NoDoRec,
                   NoRecursiveDo,
                   NoParallelListComp,
                   NoMultiParamTypeClasses,
                   NoMonomorphismRestriction,
                   NoFunctionalDependencies,
                   NoRank2Types,
                   NoRankNTypes,
                   NoPolymorphicComponents,
                   NoExistentialQuantification,
                   NoScopedTypeVariables,
                   NoPatternSignatures,
                   NoImplicitParams,
                   NoFlexibleContexts,
                   NoFlexibleInstances,
                   NoEmptyDataDecls,
                   NoCPP,
                   NoKindSignatures,
                   NoBangPatterns,
                   NoTypeSynonymInstances,
                   NoTemplateHaskell,
                   NoForeignFunctionInterface,
                   NoArrows,
                   NoGenerics,
                   NoImplicitPrelude,
                   NoNamedFieldPuns,
                   NoPatternGuards,
                   NoGeneralizedNewtypeDeriving,
                   NoExtensibleRecords,
                   NoRestrictedTypeSynonyms,
                   NoHereDocuments,
                   NoMagicHash,
                   NoTypeFamilies,
                   NoStandaloneDeriving,
                   NoUnicodeSyntax,
                   NoUnliftedFFITypes,
                   NoInterruptibleFFI,
                   NoCApiFFI,
                   NoLiberalTypeSynonyms,
                   NoTypeOperators,
                   NoRecordWildCards,
                   NoRecordPuns,
                   NoDisambiguateRecordFields,
                   NoTraditionalRecordSyntax,
                   NoOverloadedStrings,
                   NoGADTs,
                   NoGADTSyntax,
                   NoMonoPatBinds,
                   NoRelaxedPolyRec,
                   NoExtendedDefaultRules,
                   NoUnboxedTuples,
                   NoDeriveDataTypeable,
                   NoDeriveGeneric,
                   NoDefaultSignatures,
                   NoInstanceSigs,
                   NoConstrainedClassMethods,
                   NoPackageImports,
                   NoImpredicativeTypes,
                   NoNewQualifiedOperators,
                   NoPostfixOperators,
                   NoQuasiQuotes,
                   NoTransformListComp,
                   NoMonadComprehensions,
                   NoViewPatterns,
                   NoXmlSyntax,
                   NoRegularPatterns,
                   NoTupleSections,
                   NoGHCForeignImportPrim,
                   NoNPlusKPatterns,
                   NoDoAndIfThenElse,
                   NoMultiWayIf,
                   NoLambdaCase,
                   NoRebindableSyntax,
                   NoExplicitForAll,
                   NoDatatypeContexts,
                   NoMonoLocalBinds,
                   NoDeriveFunctor,
                   NoDeriveTraversable,
                   NoDeriveFoldable,
                   NoNondecreasingIndentation,
                   NoSafeImports,
                   NoSafe,
                   NoTrustworthy,
                   NoUnsafe,
                   NoConstraintKinds,
                   NoPolyKinds,
                   NoDataKinds,
                   NoParallelArrays,
                   NoRoleAnnotations,
                   NoOverloadedLists,
                   NoEmptyCase,
                   NoAutoDeriveTypeable,
                   NoNegativeLiterals,
                   NoBinaryLiterals,
                   NoNumDecimals,
                   NoNullaryTypeClasses,
                   NoExplicitNamespaces,
                   NoAllowAmbiguousTypes,
                   NoJavaScriptFFI,
                   NoPatternSynonyms,
                   NoPartialTypeSignatures,
                   NoNamedWildCards,
                   NoDeriveAnyClass,
                   NoDeriveLift,
                   NoStaticPointers,
                   NoStrictData,
                   NoStrict,
                   NoApplicativeDo,
                   NoDuplicateRecordFields,
                   NoTypeApplications,
                   NoTypeInType,
                   NoUndecidableSuperClasses,
                   NoMonadFailDesugaring,
                   NoTemplateHaskellQuotes,
                   NoOverloadedLabels,
                   NoTypeFamilyDependencies
                   ]

module Vehicle.Compile.Print.Error.Property
  ( propertyTraversalErrorDetails,
  )
where

import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendlyEmptyCtx)

propertyTraversalErrorDetails ::
  DeclProvenance ->
  MultiPropertyTraveralError ->
  VehicleError
propertyTraversalErrorDetails declProv = \case
  UnsupportedVectorDimension dim -> nonConcreteType declProv ("the dimension" <+> prettyFriendlyEmptyCtx dim)
  UnsupportedTensorDimensions dims -> nonConcreteType declProv ("the dimensions" <+> prettyFriendlyEmptyCtx dims)
  UnreducableType typ -> nonConcreteType declProv ("the type" <+> prettyFriendlyEmptyCtx typ)
  UnsupportedVectorValue value -> unreducableValue declProv "vector" (prettyFriendlyEmptyCtx value)
  UnreducableTensorValue value -> unreducableValue declProv "tensor" (prettyFriendlyEmptyCtx value)

nonConcreteType :: DeclProvenance -> UnAnnDoc -> VehicleError
nonConcreteType (ident, p) problem =
  VehicleError
    { provenance = Just p,
      problem =
        "unable to compile property"
          <+> quotePretty ident
          <+> "as unable to work out how many individual properties it contains."
          <+> "In particular, could not evaluate the"
          <+> problem,
      fix = Just "ensure that the type is fully evaluable"
    }

unreducableValue :: DeclProvenance -> UnAnnDoc -> UnAnnDoc -> VehicleError
unreducableValue (ident, p) typ value =
  VehicleError
    { provenance = Just p,
      problem =
        "unable to compile property"
          <+> quotePretty ident
          <+> "as unable to reduce the value"
          <+> value
          <+> "to a concrete"
          <+> typ
          <+> "of individual properties",
      fix = Just $ "ensure that the" <+> typ <+> "is fully evaluable"
    }

module Component.MetadataTab.View (metadataView) where

import Prologue hiding (div, min)

import Component.MetadataTab.Types (MetadataAction(..))
import Contrib.Data.Array.Builder (build, cons) as AB
import Contrib.Data.List.Infinite.Finalize (zip) as Infinite.Finalize
import Contrib.Halogen.Components.Sortable (DragHandlers(..), GenDragHandlers)
import Data.Array (concat, concatMap)
import Data.Array (length) as Array
import Data.Foldable (foldMap, foldMapDefaultL)
import Data.Int as Int
import Data.Lens (Lens', (^.))
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Map.Ordered.OMap (OMap)
import Data.Map.Ordered.OMap as OMap
import Data.Set (Set, toUnfoldable)
import Data.Set.Ordered.OSet (OSet)
import Data.Tuple.Nested (type (/\), (/\))
import Halogen.Classes (btn, minusBtn, plusBtn)
import Halogen.HTML
  ( ClassName(..)
  , HTML
  , button
  , div
  , div_
  , em_
  , h6_
  , input
  , option
  , select
  , text
  )
import Halogen.HTML.Events (onClick, onValueChange)
import Halogen.HTML.Properties
  ( InputType(..)
  , class_
  , classes
  , draggable
  , min
  , placeholder
  , required
  , selected
  , type_
  , value
  )
import Marlowe.Extended
  ( contractTypeArray
  , contractTypeInitials
  , contractTypeName
  , initialsToContractType
  )
import Marlowe.Extended.Metadata
  ( ChoiceInfo
  , MetaData
  , MetadataHintInfo
  , NumberFormat(..)
  , NumberFormatType(..)
  , _choiceInfo
  , _choiceNames
  , _roleDescriptions
  , _roles
  , _slotParameterDescriptions
  , _slotParameters
  , _valueParameterInfo
  , _valueParameters
  , defaultForFormatType
  , fromString
  , getFormatType
  , isDecimalFormat
  , isDefaultFormat
  , toString
  )

onlyDescriptionRenderer
  :: forall a p r
   . { delete :: String -> a, set :: String -> String -> a | r }
  -> String
  -> String
  -> Boolean
  -> String
  -> String
  -> Array (HTML p a)
onlyDescriptionRenderer
  actions
  key
  info
  needed
  typeNameTitle
  typeNameSmall =
  [ div [ class_ $ ClassName "metadata-prop-label" ]
      [ text $ typeNameTitle <> " " <> show key <> ": " ]
  , div [ class_ $ ClassName "metadata-prop-edit" ]
      [ input
          [ type_ InputText
          , placeholder $ "Description for " <> typeNameSmall <> " " <> show key
          , class_ $ ClassName "metadata-input"
          , value info
          , onValueChange $ actions.set key
          ]
      ]
  , div [ class_ $ ClassName "metadata-prop-delete" ]
      [ button
          [ classes
              [ if needed then plusBtn else minusBtn
              , ClassName "align-top"
              , btn
              ]
          , onClick $ const $ actions.delete key
          ]
          [ text "-" ]
      ]
  ]
    <>
      if needed then []
      else
        [ div
            [ classes
                [ ClassName "metadata-error"
                , ClassName "metadata-prop-not-used"
                ]
            ]
            [ text "Not used" ]
        ]

type FormattedNumberInfo a
  =
  { key :: String
  , description :: String
  , format :: NumberFormat
  , setFormat :: String -> NumberFormat -> a
  , setDescription :: String -> String -> a
  , deleteInfo :: String -> a
  }

formattedNumberMetadataRenderer
  :: forall a p
   . FormattedNumberInfo a
  -> Boolean
  -> String
  -> String
  -> Array (HTML p a)
formattedNumberMetadataRenderer
  { key, description, format, setFormat, setDescription, deleteInfo }
  needed
  typeNameTitle
  typeNameSmall =
  [ div [ class_ $ ClassName "metadata-prop-label" ]
      [ text $ typeNameTitle <> " " <> show key <> ": " ]
  , div [ class_ $ ClassName "metadata-prop-formattednum-col1" ]
      [ select
          [ class_ $ ClassName "metadata-input"
          , onValueChange $ setFormat key <<< setNumberFormatType
          ]
          [ option
              [ value $ toString DefaultFormatType
              , selected $ isDefaultFormat format
              ]
              [ text $ "Default format"
              ]
          , option
              [ value $ toString DecimalFormatType
              , selected $ isDecimalFormat format
              ]
              [ text $ "Fixed point amount"
              ]
          ]
      ]
  , div [ class_ $ ClassName "metadata-prop-formattednum-col2" ]
      [ input
          [ type_ InputText
          , placeholder $ "Description for " <> typeNameSmall <> " " <> show key
          , class_ $ ClassName "metadata-input"
          , value description
          , onValueChange $ setDescription key
          ]
      ]
  , div [ class_ $ ClassName "metadata-prop-delete" ]
      [ button
          [ classes
              [ if needed then plusBtn else minusBtn
              , ClassName "align-top"
              , btn
              ]
          , onClick $ const $ deleteInfo key
          ]
          [ text "-" ]
      ]
  ]
    <>
      ( if needed then []
        else
          [ div
              [ classes
                  [ ClassName "metadata-error"
                  , ClassName "metadata-prop-not-used"
                  ]
              ]
              [ text "Not used" ]
          ]
      )
    <> case format of
      DefaultFormat -> []
      DecimalFormat numDecimals labelStr ->
        [ div [ class_ $ ClassName "metadata-prop-formattednum-col1" ]
            [ input
                [ type_ InputNumber
                , placeholder $ "Number of decimal digits for " <> typeNameSmall
                    <> " "
                    <> show key
                , class_ $ ClassName "metadata-input"
                , value $ if numDecimals == 0 then "" else show numDecimals
                , required true
                , min zero
                , onValueChange $ setFormat key <<< setDecimals labelStr
                ]
            ]
        , div [ class_ $ ClassName "metadata-prop-formattednum-col2" ]
            [ input
                [ type_ InputText
                , placeholder $ "Currency label for " <> typeNameSmall <> " " <>
                    show key
                , class_ $ ClassName "metadata-input"
                , value labelStr
                , onValueChange $ setFormat key <<< DecimalFormat numDecimals
                ]
            ]
        ]
      TimeFormat -> []
  where
  setNumberFormatType :: String -> NumberFormat
  setNumberFormatType str = case fromString str of
    Just formatType
      | formatType == getFormatType format -> format
      | otherwise -> defaultForFormatType formatType
    Nothing -> defaultForFormatType DefaultFormatType

  setDecimals :: String -> String -> NumberFormat
  setDecimals labelStr x =
    DecimalFormat
      ( case Int.fromString x of
          Just y
            | y >= 0 -> y
          _ -> 0
      )
      labelStr

choiceMetadataRenderer
  :: forall p
   . String
  -> ChoiceInfo
  -> Boolean
  -> String
  -> String
  -> Array (HTML p MetadataAction)
choiceMetadataRenderer key { choiceDescription, choiceFormat } =
  formattedNumberMetadataRenderer
    { key: key
    , description: choiceDescription
    , format: choiceFormat
    , setFormat: SetChoiceFormat
    , setDescription: SetChoiceDescription
    , deleteInfo: DeleteChoiceInfo
    }

metadataList
  :: forall a c p r v
   . Monoid v
  => { set :: String -> v -> a | r }
  -> Map String c
  -> Set String
  -> (String -> c -> Boolean -> String -> String -> Array (HTML p a))
  -> String
  -> String
  -> Array (HTML p a)
metadataList
  actions
  metadataMap
  hintSet
  metadataRenderer
  typeNameTitle
  typeNameSmall =
  if Map.isEmpty combinedMap then
    []
  else
    [ div [ class_ $ ClassName "metadata-group-title" ]
        [ h6_ [ em_ [ text $ typeNameTitle <> " descriptions" ] ] ]
    ]
      <>
        ( concatMap
            ( \(key /\ val) ->
                ( case val of
                    Just (info /\ needed) -> metadataRenderer key info needed
                      typeNameTitle
                      typeNameSmall
                    Nothing ->
                      [ div
                          [ classes
                              [ ClassName "metadata-error"
                              , ClassName "metadata-prop-not-defined"
                              ]
                          ]
                          [ text $ typeNameTitle <> " " <> show key <>
                              " meta-data not defined"
                          ]
                      , div [ class_ $ ClassName "metadata-prop-create" ]
                          [ button
                              [ classes [ minusBtn, ClassName "align-top", btn ]
                              , onClick $ const $ (actions.set key mempty)
                              ]
                              [ text "+" ]
                          ]
                      ]
                )
            )
            $ Map.toUnfoldable combinedMap
        )
  where
  mergeMaps
    :: forall c2
     . (Maybe (c2 /\ Boolean))
    -> (Maybe (c2 /\ Boolean))
    -> (Maybe (c2 /\ Boolean))
  mergeMaps (Just (x /\ _)) _ = Just (x /\ true)

  mergeMaps _ _ = Nothing

  -- The value of the Map has the following meaning:
  -- * Nothing means the entry is in the contract but not in the metadata
  -- * Just (_ /\ false) means the entry is in the metadata but not in the contract
  -- * Just (_ /\ true) means the entry is both in the contract and in the metadata
  -- If it is nowhere we just don't store it in the map
  combinedMap =
    Map.unionWith mergeMaps
      (map (\x -> Just (x /\ false)) metadataMap)
      ( Map.fromFoldable
          (map (\x -> x /\ Nothing) ((toUnfoldable hintSet) :: List String))
      )

sortableMetadataList
  :: forall a c p r v
   . Monoid v
  => { dragging :: DraggingState a
     , set :: String -> v -> a
     | r
     }
  -> OMap String c
  -> OSet String
  -> (String -> c -> Boolean -> String -> String -> Array (HTML p a))
  -> String
  -> String
  -> Array (HTML p a)
sortableMetadataList
  actions
  metadataMap
  hintSet
  metadataRenderer
  typeNameTitle
  typeNameSmall = do
  let
    items :: Array _
    items = Infinite.Finalize.zip
      actions.dragging.genDragHandlers
      (OMap.toUnfoldable combinedMap :: Array _)
    itemsCount = Array.length items

    sortableRow h = div $
      if itemsCount > 1 then
        [ h.onDragEnd
        , h.onDragStart
        , h.onDragEnter
        , class_ $ ClassName "metadata-sortable-form-row"
        , draggable true
        ]
      else
        [ class_ $ ClassName "metadata-sortable-form-row" ]
  if OMap.isEmpty combinedMap then
    []
  else AB.build
    $
      AB.cons do
        div [ class_ $ ClassName "metadata-group-title" ]
          [ h6_ [ em_ [ text $ typeNameTitle <> " descriptions" ] ] ]
        <> items `flip foldMapDefaultL` \(DragHandlers h /\ key /\ val) ->
          case val of
            Just (info /\ needed) -> AB.cons $ sortableRow h
              $ metadataRenderer key info needed typeNameTitle typeNameSmall
                  <>
                    [ button
                        [ classes [ btn, ClassName "move-btn" ] ]
                        [ text "↕" ]
                    ]
            Nothing -> AB.cons $ div
              [ class_ $ ClassName "metadata-sortable-form-row" ]
              [ div
                  [ classes
                      [ ClassName "metadata-error"
                      , ClassName "metadata-prop-not-defined"
                      ]
                  ]
                  [ text $ typeNameTitle <> " " <> show key <>
                      " meta-data not defined"
                  ]
              , div [ class_ $ ClassName "metadata-prop-create" ]
                  [ button
                      [ classes [ minusBtn, ClassName "align-top", btn ]
                      , onClick $ const $ actions.set key mempty
                      ]
                      [ text "+" ]
                  ]
              ]
  where
  mergeMaps
    :: forall c2
     . (Maybe (c2 /\ Boolean))
    -> (Maybe (c2 /\ Boolean))
    -> (Maybe (c2 /\ Boolean))
  mergeMaps (Just (x /\ _)) _ = Just (x /\ true)

  mergeMaps _ _ = Nothing

  -- The value of the Map has the following meaning:
  -- * Nothing means the entry is in the contract but not in the metadata
  -- * Just (_ /\ false) means the entry is in the metadata but not in the contract
  -- * Just (_ /\ true) means the entry is both in the contract and in the metadata
  -- If it is nowhere we just don't store it in the map
  combinedMap =
    OMap.unionWith mergeMaps
      (map (\x -> Just (x /\ false)) metadataMap)
      (foldMap (\x -> OMap.singleton x Nothing) hintSet)

type DraggingState a =
  { dragged :: Maybe Int
  , genDragHandlers :: GenDragHandlers a
  }

type Dragging a =
  { slotParameterDescriptions :: DraggingState a
  , valueParameterInfos :: DraggingState a
  }

type Handlers a =
  { raise :: MetadataAction -> a
  , dragging :: Dragging a
  }

metadataView
  :: forall a p
   . Handlers a
  -> MetadataHintInfo
  -> MetaData
  -> HTML p a
metadataView handlers metadataHints metadata = div_
  [ div [ classes [ ClassName "metadata-form" ] ] $ concat
      [ [ div [ class_ $ ClassName "metadata-mainprop-label" ]
            [ text "Contract type: " ]
        , div [ class_ $ ClassName "metadata-mainprop-edit" ]
            [ select
                [ class_ $ ClassName "metadata-input"
                , onValueChange $ handlers.raise <<< SetContractType <<<
                    initialsToContractType
                ]
                do
                  ct <- contractTypeArray
                  pure
                    $ option
                        [ value $ contractTypeInitials ct
                        , selected (ct == metadata.contractType)
                        ]
                        [ text $ contractTypeName ct
                        ]
            ]
        ]
      , [ div [ class_ $ ClassName "metadata-mainprop-label" ]
            [ text "Contract name: " ]
        , div [ class_ $ ClassName "metadata-mainprop-edit" ]
            [ input
                [ type_ InputText
                , placeholder "Contract name"
                , class_ $ ClassName "metadata-input"
                , value metadata.contractName
                , onValueChange $ handlers.raise <<< SetContractName
                ]
            ]
        ]
      , [ div [ class_ $ ClassName "metadata-mainprop-label" ]
            [ text "Contract short description: " ]
        , div [ class_ $ ClassName "metadata-mainprop-edit" ]
            [ input
                [ type_ InputText
                , placeholder "Contract description"
                , class_ $ ClassName "metadata-input"
                , value metadata.contractShortDescription
                , onValueChange $ handlers.raise <<< SetContractShortDescription
                ]
            ]
        ]
      , [ div [ class_ $ ClassName "metadata-mainprop-label" ]
            [ text "Contract long description: " ]
        , div [ class_ $ ClassName "metadata-mainprop-edit" ]
            [ input
                [ type_ InputText
                , placeholder "Contract description"
                , class_ $ ClassName "metadata-input"
                , value metadata.contractLongDescription
                , onValueChange $ handlers.raise <<< SetContractLongDescription
                ]
            ]
        ]
      ]
  , div [ classes [ ClassName "metadata-form" ] ] do
      let
        actions =
          { delete: handlers.raise <<< DeleteRoleDescription
          , set: map handlers.raise <<< SetRoleDescription
          }
        render = onlyDescriptionRenderer actions
      generateMetadataList
        actions
        _roleDescriptions
        _roles
        render
        "Role"
        "role"
  , div [ classes [ ClassName "metadata-form" ] ] do
      let
        actions = { set: SetChoiceDescription }

      map handlers.raise <$> generateMetadataList
        actions
        _choiceInfo
        _choiceNames
        choiceMetadataRenderer
        "Choice"
        "choice"
  , div [ classes [ ClassName "sortable-metadata-form" ] ] do
      let
        actions =
          { delete: handlers.raise <<< DeleteSlotParameterDescription
          , dragging: handlers.dragging.slotParameterDescriptions
          , set: map handlers.raise <<< SetSlotParameterDescription
          }
        render = onlyDescriptionRenderer actions
      generateSortableMetadataList
        actions
        _slotParameterDescriptions
        _slotParameters
        render
        "Slot parameter"
        "slot parameter"
  , div [ classes [ ClassName "sortable-metadata-form" ] ] do
      let
        actions =
          { set: map handlers.raise <<< SetValueParameterDescription
          , dragging: handlers.dragging.valueParameterInfos
          }
        render key vp = formattedNumberMetadataRenderer
          { key: key
          , description: vp.valueParameterDescription
          , format: vp.valueParameterFormat
          , setFormat: map handlers.raise <<< SetValueParameterFormat
          , setDescription: map handlers.raise <<< SetValueParameterDescription
          , deleteInfo: handlers.raise <<< DeleteValueParameterInfo
          }

      generateSortableMetadataList
        actions
        _valueParameterInfo
        _valueParameters
        render
        "Value parameter"
        "value parameter"
  ]
  where
  generateMetadataList
    :: forall b c v r
     . Monoid v
    => { set :: String -> v -> b | r }
    -> Lens' MetaData (Map String c)
    -> Lens' MetadataHintInfo (Set String)
    -> ( String
         -> c
         -> Boolean
         -> String
         -> String
         -> Array (HTML p b)
       )
    -> String
    -> String
    -> Array (HTML p b)
  generateMetadataList actions mapLens setLens =
    metadataList actions (metadata ^. mapLens) (metadataHints ^. setLens)

  generateSortableMetadataList
    :: forall c r v
     . Monoid v
    => { dragging :: DraggingState a
       , set :: String -> v -> a
       | r
       }
    -> Lens' MetaData (OMap String c)
    -> Lens' MetadataHintInfo (OSet String)
    -> ( String
         -> c
         -> Boolean
         -> String
         -> String
         -> Array (HTML p a)
       )
    -> String
    -> String
    -> Array (HTML p a)
  generateSortableMetadataList actions mapLens setLens render label labelLower =
    sortableMetadataList
      actions
      (metadata ^. mapLens)
      (metadataHints ^. setLens)
      render
      label
      labelLower


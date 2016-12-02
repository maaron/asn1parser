// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#r "../packages/FParsec.1.0.2/lib/net40-client/FParsecCS.dll"
#r "../packages/FParsec.1.0.2/lib/net40-client/FParsec.dll"

open FParsec

module Ast =
    type ModuleDefinition = {
        moduleIdentifier: ModuleIdentifier
        encodingReference: string option
        tagDefault: TagDefault
        extensionDefault: ExtensionDefault
        }

    and ModuleIdentifier = {
        reference: string
        definitive: DefinitiveIdentifier
        }

    and DefinitiveIdentifier = {
        oid: string list
        iri: string list option
        }

    and TagDefault = Explicit | Implicit | Automatic | Empty

    and ExtensionDefault = Implied | Empty

    and ModuleBody = {
        imports: Imports
        exports: Exports
        assignmentList: Assignment list
        }

    and Exports =
        | All
        | Symbols of Symbol list
        | Empty

    and Symbol = string

    and Assignment = {
        valuereference: string
        definition: Value
        }

    and AssignedIdentifier =
        | ObjectIdentifierValue of OID
        | DefinedValue of DefinedValue
        | Empty

    and OID = DefinedValue option * ObjIdComponent list

    and DefinedValue =
        | ExternalValueReference of ExternalValueReference
        | ValueReference of string

    and ExternalValueReference = {
        moduleReference: string
        valueReference: string list
        }

    and ObjIdComponent =
        | NumberForm of string
        | NameForm of string
        | NameAndNumberForm of string * string
        | DefinedValue of DefinedValue

    and Imports =
        | Symbols of SymbolsFromModule list
        | Empty

    and SymbolsFromModule = {
        moduleReference: string * AssignedIdentifier
        symbols: Symbol list
        }

    and TypeAssignment = {
        typereference: string
        definition: Type
        }

    and Type = unit

    and BitStringValue =
        | Bstring of string
        | Hstring of string
        | IdentifierList of string list
        | Containing of Value
    
    and Value = 
        | BitStringValue of BitStringValue
        | BooleanValue of bool
        | CharacterStringValue of string

    and CharacterStringType =
        | Unrestricted
        | BMPString
        | GeneralString
        | GraphicString
        | IA5String
        | ISO646String
        | NumericString
        | PrintableString
        | TeletexString
        | T61String
        | UniversalString
        | UTF8String
        | VideotexString
        | VisibleString

    and BuiltinType =
        | BitStringType
        | BooleanType
        | CharacterStringType of CharacterStringType
        | ChoiceType
        | DateType
        | DateTimeType
        | DurationType
        | EmbeddedPDVType
        | EnumeratedType
        | ExternalType
        | InstanceOfType
        | IntegerType
        | IRIType
        | NullType
        | ObjectClassFieldType
        | ObjectIdentifierType
        | OctetStringType
        | RealType
        | RelativeIRIType
        | RelativeOIDType
        | SequenceType
        | SequenceOfType
        | SetType
        | SetOfType
        | PrefixedType
        | TimeType
        | TimeOfDayType

    and ExceptionIdentification =
        | SignedNumber of int
        | DefinedValue of DefinedValue
        | TypeValue of Type * Value

open Ast

let str = pstring

let betweenBraces p = between (pchar '{') (pchar '}') p
let betweenParens p = between (pchar '(') (pchar ')') p
let betweenDoubleQuotes p = between (pchar '"') (pchar '"') p
let betweenQuotes p = between (pchar '\'') (pchar '\'') p

let EXPLICIT = str "EXPLICIT"
let IMPLICIT = str "IMPLICIT"
let TAGS = str "TAGS"
let INSTRUCTIONS = str "INSTRUCTIONS"
let AUTOMATIC = str "AUTOMATIC"
let EXTENSIBILITY = str "EXTENSIBILITY"
let IMPLIED = str "IMPLIED"
let EXPORTS = str "EXPORTS"
let ALL = str "ALL"
let FROM = str "FROM"
let IMPORTS = str "IMPORTS"
let BIT = str "BIT"
let STRING = str "STRING"
let CHARACTER = str "CHARACTER"
let BOOLEAN = str "BOOLEAN"
let BMPString = str "BMPString"
let GeneralString = str "GeneralString"
let GraphicString = str "GraphicString"
let IA5String = str "IA5String"
let ISO646String = str "ISO646String"
let NumericString = str "NumericString"
let PrintableString = str "PrintableString"
let TeletexString = str "TeletexString"
let T61String = str "T61String"
let UniversalString = str "UniversalString"
let UTF8String = str "UTF8String"
let VideotexString = str "VideotexString"
let VisibleString = str "VisibleString"
let CHOICE = str "CHOICE"
let threeDots = str "..."
let CONTAINING = str "CONTAINING"
let TRUE = str "TRUE"
let FALSE = str "FALSE"

(* Recursive rules *)
let Type, TypeRef = createParserForwardedToRef()
let Value, ValueRef = createParserForwardedToRef()

let empty = preturn ()

let typereference = manyChars2 asciiUpper (asciiLetter <|> digit <|> (pchar '-' .>> notFollowedBy (pchar '-')))

let identifier = manyChars2 asciiLower (asciiLetter <|> digit <|> (pchar '-' .>> notFollowedBy (pchar '-')))

let encodingreference = manyChars asciiUpper

let ArcIdentifier = manySatisfy ((<>) '/')

let modulereference = typereference

let valuereference = identifier

let NameForm = identifier

let number = manyChars digit

let DefinitiveNumberForm = number

let DefinitiveNameAndNumberForm = identifier .>> betweenParens DefinitiveNumberForm

let DefinitiveObjIdComponent =
    NameForm
    <|> DefinitiveNumberForm
    <|> DefinitiveNameAndNumberForm

let DefinitiveObjIdComponentList =
    sepBy DefinitiveObjIdComponent spaces1

let DefinitiveOID =
    DefinitiveObjIdComponentList |> betweenBraces

let IRIValue =
    (sepBy ArcIdentifier (pchar '/'))
    |> betweenDoubleQuotes

let DefinitiveOIDandIRI = DefinitiveOID .>>. IRIValue

let DefinitiveIdentification = parse {
    let! oid = DefinitiveOID
    let! iri = opt IRIValue

    return {
        oid = oid
        iri = iri
        }
    }

let EncodingReferenceDefault =
    encodingreference .>> str "INSTRUCTIONS" |> opt

let TagDefault =
    (EXPLICIT >>% Ast.Explicit) .>> TAGS
    <|> (IMPLICIT >>% Ast.Implicit) .>> TAGS
    <|> (AUTOMATIC >>% Ast.Automatic) .>> TAGS
    <|> preturn Ast.Empty

let ExtensionDefault =
    (EXTENSIBILITY >>. IMPLIED) >>% Ast.Implied
    <|> preturn Ast.ExtensionDefault.Empty

let ModuleIdentifier = parse {
    let! ref = modulereference
    let! defId = DefinitiveIdentification

    return {
        reference = ref
        definitive = defId
        }
    }

let Reference =
    typereference
    <|> valuereference
    (* TODO
    <|> objectclassreference
    <|> objectreference
    <|> objectsetreference
    *)

let Symbol =
    Reference
    (*
    <|> ParameterizedReference
    *)

let SymbolList =
    sepBy Symbol (pchar ',')

let Exports =
    (EXPORTS >>. SymbolList .>> (pchar ';')) |>> Ast.Exports.Symbols
    <|> ((EXPORTS >>. ALL .>> (pchar ';')) >>% Ast.Exports.All)
    <|> preturn Ast.Exports.Empty

let ExternalValueReference =
    pipe2 (modulereference .>> pchar '.') (sepBy valuereference (pchar '.'))
        (fun m vlist -> { moduleReference = m; valueReference = vlist})

let DefinedValue =
    (ExternalValueReference |>> Ast.ExternalValueReference)
    <|> (valuereference |>> Ast.ValueReference)

let NumberForm = number

let NameAndNumberForm =
    identifier .>>. betweenParens NumberForm

let ObjIdComponent =
    (NameForm |>> Ast.ObjIdComponent.NameForm)
    <|> (NumberForm |>> Ast.ObjIdComponent.NumberForm)
    <|> (NameAndNumberForm |>> Ast.ObjIdComponent.NameAndNumberForm)
    <|> (DefinedValue |>> Ast.ObjIdComponent.DefinedValue)

let ObjIdComponentsList =
    many1 ObjIdComponent

let ObjectIdentifierValue =
    (opt DefinedValue .>>. ObjIdComponentsList)
    |> betweenBraces 

let AssignedIdentifier =
    (ObjectIdentifierValue |>> Ast.AssignedIdentifier.ObjectIdentifierValue)
    <|> (DefinedValue |>> Ast.AssignedIdentifier.DefinedValue)
    <|> preturn Ast.AssignedIdentifier.Empty

let GlobalModuleReference =
    modulereference .>>. AssignedIdentifier

let SymbolsFromModule =
    pipe2 (SymbolList .>> FROM) GlobalModuleReference
        (fun s m -> { moduleReference = m; symbols = s })

let SymbolsFromModuleList =
    many SymbolsFromModule

let Imports =
    (IMPORTS >>. SymbolsFromModuleList .>> (pchar ';') |>> Ast.Imports.Symbols)
    <|> preturn Ast.Imports.Empty

let BitStringType =
    BIT .>> STRING >>% Ast.BitStringType
    (*
    <|> BIT STRING "{" NamedBitList "}"
    *)

let BooleanType = BOOLEAN >>% Ast.BooleanType

let RestrictedCharacterStringType =
    (BMPString >>% Ast.Unrestricted)
    <|> (GeneralString >>% Ast.GeneralString)
    <|> (GraphicString >>% Ast.GraphicString)
    <|> (IA5String >>% Ast.IA5String)
    <|> (ISO646String >>% Ast.ISO646String)
    <|> (NumericString >>% Ast.NumericString)
    <|> (PrintableString >>% Ast.PrintableString)
    <|> (TeletexString >>% Ast.TeletexString)
    <|> (T61String >>% Ast.T61String)
    <|> (UniversalString >>% Ast.UniversalString)
    <|> (UTF8String >>% Ast.UTF8String)
    <|> (VideotexString >>% Ast.VideotexString)
    <|> (VisibleString >>% Ast.VisibleString)

let UnrestrictedCharacterStringType = CHARACTER .>> STRING

let CharacterStringType =
    RestrictedCharacterStringType 
    <|> (UnrestrictedCharacterStringType >>% Ast.Unrestricted)

let NamedType = identifier .>>. Type

let AlternativeTypeList =
    sepBy1 NamedType (pchar ',')

let SignedNumber =
    pipe2 (opt (pchar '-') |>> Option.isSome) number
         (fun sign num -> 
            let n = System.Int32.Parse(num)
            if sign then -n else n)

let bstring = 
    manySatisfy (fun c -> c = '0' || c = '1')
    |> betweenQuotes
    .>> (pchar 'B')

let hstring = 
    manySatisfy (fun c -> isDigit c || isAnyOf "ABCDEF" c)
    |> betweenQuotes
    .>> (pchar 'H')

let IdentifierList =
    sepBy identifier (pchar ',')

let BitStringValue =
    (bstring |>> Ast.Bstring)
    <|> (hstring |>> Ast.Hstring)
    <|> (betweenBraces IdentifierList |>> Ast.IdentifierList)
    <|> (CONTAINING >>. Value |>> Ast.Containing)

let BooleanValue = 
    TRUE >>% true
    <|> (FALSE >>% false)

let cstring =
    betweenDoubleQuotes (manyChars ((satisfy ((<>) '"')) <|> (pchar '"' .>> pchar '"')))
    
let foo = manyChars ((satisfy ((<>) '"')) <|> (pchar '"' .>>? pchar '"'))
runParserOnString foo () "test" "a\"\"\"bd"

runParserOnString cstring () "test" "\"abc\""

let RestrictedCharacterStringValue =
    cstring
    <|> CharacterStringList
    <|> Quadruple
    <|> Tuple

let UnrestrictedCharacterStringValue = SequenceValue

let CharacterStringValue =
    RestrictedCharacterStringValue
    <|> UnrestrictedCharacterStringValue

let BuiltinValue =
    (BitStringValue |>> Ast.BitStringValue)
    <|> (BooleanValue |>> Ast.BooleanValue)
    <|> (CharacterStringValue |>> Ast.CharacterStringValue)
    <|> ChoiceValue
    <|> EmbeddedPDVValue
    <|> EnumeratedValue
    <|> ExternalValue
    <|> InstanceOfValue
    <|> IntegerValue
    <|> IRIValue
    <|> NullValue
    <|> ObjectIdentifierValue
    <|> OctetStringValue
    <|> RealValue
    <|> RelativeIRIValue
    <|> RelativeOIDValue
    <|> SequenceValue
    <|> SequenceOfValue
    <|> SetValue
    <|> SetOfValue
    <|> PrefixedValue
    <|> TimeValue

let ValueRef =
    BuiltinValue
    <|> ReferencedValue
    <|> ObjectClassFieldValue

let ExceptionIdentification =
    (SignedNumber |>> Ast.ExceptionIdentification.SignedNumber)
    <|> (DefinedValue |>> Ast.ExceptionIdentification.DefinedValue)
    <|> ((Type .>> pchar ':' >>. Value) |>> Ast.ExceptionIdentification.TypeValue)

let ExceptionSpec = opt ((pchar '!') >>. ExceptionIdentification)

let ExtensionAndException = threeDots .>> opt ExceptionSpec

let AlternativeTypeLists =
    AlternativeTypeList 
    .>>. (opt (pchar ',') 
        .>>. ExtensionAndException 
        .>>. ExtensionAdditionAlternatives 
        .>>. OptionalExtensionMarker)

let ChoiceType = CHOICE >>. betweenBraces AlternativeTypeLists

ExtensionAdditionAlternatives ::=
    "," ExtensionAdditionAlternativesList
    <|> empty

ExtensionAdditionAlternativesList ::=
    ExtensionAdditionAlternative
    <|> ExtensionAdditionAlternativesList "," ExtensionAdditionAlternative

ExtensionAdditionAlternative ::=
    ExtensionAdditionAlternativesGroup
    <|> NamedType

ExtensionAdditionAlternativesGroup ::=
    "[[" VersionNumber AlternativeTypeList "]]"

let BuiltinType =
    BitStringType
    <|> BooleanType
    <|> (CharacterStringType |>> Ast.BuiltinType.CharacterStringType)
    <|> ChoiceType
    <|> DateType
    <|> DateTimeType
    <|> DurationType
    <|> EmbeddedPDVType
    <|> EnumeratedType
    <|> ExternalType
    <|> InstanceOfType
    <|> IntegerType
    <|> IRIType
    <|> NullType
    <|> ObjectClassFieldType
    <|> ObjectIdentifierType
    <|> OctetStringType
    <|> RealType
    <|> RelativeIRIType
    <|> RelativeOIDType
    <|> SequenceType
    <|> SequenceOfType
    <|> SetType
    <|> SetOfType
    <|> PrefixedType
    <|> TimeType
    <|> TimeOfDayType

let TypeRef = 
    BuiltinType 
    (*
    <|> ReferencedType 
    <|> ConstrainedType
    *)

let TypeAssignment =
    pipe2 (typereference .>> str "::=") Type
        (fun id t -> { typereference = id; typeDefinition = t })

ValueAssignment ::=
    valuereference Type "::=" Value

let Assignment =
    TypeAssignment
    <|> ValueAssignment
    (*
    <|> XMLValueAssignment
    <|> ValueSetTypeAssignment
    <|> ObjectClassAssignment
    <|> ObjectAssignment
    <|> ObjectSetAssignment
    <|> ParameterizedAssignment
    *)

let AssignmentList =
    many Assignment

let ModuleBody =
    pipe3 Exports Imports AssignmentList (fun e i a -> { exports = e; imports = i; assignmentList = a })

let ModuleDefinition = parse {
    let! id = ModuleIdentifier
    let! _ = str "DEFINITIONS"
    let! encoding = EncodingReferenceDefault
    let! tag = TagDefault
    let! ext = ExtensionDefault
    let! _ = str "::="
    let! _ = str "BEGIN"
    (*
    >> ModuleBody
    >> EncodingControlSections
    *)
    let! _ = str "END"

    return {
        moduleIdentifier = id
        encodingReference = encoding
        tagDefault = tag
        extensionDefault = ext
        }
    }

SymbolsImported ::=
    SymbolsFromModuleList
    <|> empty

DefinedType ::=
    ExternalTypeReference
    <|> typereference
    <|> ParameterizedType
    <|> ParameterizedValueSetType

NonParameterizedTypeName ::=
    ExternalTypeReference
    <|> typereference
    <|> xmlasn1typename

ExternalTypeReference ::=
    modulereference
    "."
    typereference

AbsoluteReference ::=
    "@" ModuleIdentifier
    "."
    ItemSpec

ItemSpec ::=
    typereference
    <|> ItemId "." ComponentId

ItemId ::= ItemSpec

ComponentId ::=
    identifier
    <|> number
    <|> "*"

XMLValueAssignment ::=
    valuereference
"::="
    XMLTypedValue

XMLTypedValue ::=
    "<" & NonParameterizedTypeName ">"
    XMLValue
    "</" & NonParameterizedTypeName ">"
    <|> "<" & NonParameterizedTypeName "/>"

ValueSetTypeAssignment ::=
    typereference
    Type
"::="
    ValueSet

ValueSet ::= "{" ElementSetSpecs "}"

ReferencedType ::=
    DefinedType
    <|> UsefulType
    <|> SelectionType
    <|> TypeFromObject
    <|> ValueSetFromObjects

XMLValue ::=
    XMLBuiltinValue
    <|> XMLObjectClassFieldValue

XMLBuiltinValue ::=
    XMLBitStringValue
    <|> XMLBooleanValue
    <|> XMLCharacterStringValue
    <|> XMLChoiceValue
    <|> XMLEmbeddedPDVValue
    <|> XMLEnumeratedValue
    <|> XMLExternalValue
    <|> XMLInstanceOfValue
    <|> XMLIntegerValue
    <|> XMLIRIValue
    <|> XMLNullValue
    <|> XMLObjectIdentifierValue
    <|> XMLOctetStringValue
    <|> XMLRealValue
    <|> XMLRelativeIRIValue
    <|> XMLRelativeOIDValue
    <|> XMLSequenceValue
    <|> XMLSequenceOfValue
    <|> XMLSetValue
    <|> XMLSetOfValue
    <|> XMLPrefixedValue
    <|> XMLTimeValue

ReferencedValue ::=
    DefinedValue
    <|> ValueFromObject

NamedValue ::= identifier Value

XMLNamedValue ::= "<" & identifier ">" XMLValue "</" & identifier ">"

XMLBooleanValue ::=
    EmptyElementBoolean
    <|> TextBoolean

EmptyElementBoolean ::=
    "<" & "true" "/>"
    <|> "<" & "false" "/>"

TextBoolean ::=
    extended-true
    <|> extended-false

IntegerType ::=
    INTEGER
    <|> INTEGER "{" NamedNumberList "}"

NamedNumberList ::=
    NamedNumber
    <|> NamedNumberList "," NamedNumber

NamedNumber ::=
    identifier "(" SignedNumber ")"
    <|> identifier "(" DefinedValue ")"

IntegerValue ::=
    SignedNumber
    <|> identifier

XMLIntegerValue ::=
    XMLSignedNumber
    <|> EmptyElementInteger
    <|> TextInteger

XMLSignedNumber ::=
    number
    <|> "-" & number

EmptyElementInteger ::=
    <|> "<" & identifier "/>"

TextInteger ::=
    identifier

EnumeratedType ::=
    ENUMERATED "{" Enumerations "}"

Enumerations ::=
    RootEnumeration
    <|> RootEnumeration "," "..." ExceptionSpec
    <|> RootEnumeration "," "..." ExceptionSpec "," AdditionalEnumeration

RootEnumeration ::= Enumeration

AdditionalEnumeration ::= Enumeration

Enumeration ::= EnumerationItem <|> EnumerationItem "," Enumeration

EnumerationItem ::= identifier <|> NamedNumber

EnumeratedValue ::= identifier

XMLEnumeratedValue ::=
    EmptyElementEnumerated
    <|> TextEnumerated

EmptyElementEnumerated ::= "<" & identifier "/>"

TextEnumerated ::= identifier

RealType ::= REAL

RealValue ::=
    NumericRealValue
    <|> SpecialRealValue

NumericRealValue ::=
    realnumber
    <|> "-" realnumber
    <|> SequenceValue

SpecialRealValue ::=
    PLUS-INFINITY
    <|> MINUS-INFINITY
    <|> NOT-A-NUMBER

XMLRealValue ::=
    XMLNumericRealValue <|> XMLSpecialRealValue

XMLNumericRealValue ::=
    realnumber
    <|> "-" & realnumber

XMLSpecialRealValue ::=
    EmptyElementReal
    <|> TextReal

EmptyElementReal ::=
    "<" & PLUS-INFINITY "/>"
    <|> "<" & MINUS-INFINITY "/>"
    <|> "<" & NOT-A-NUMBER "/>"

TextReal ::=
    "INF"
    <|> "-" & "INF"
    <|> "NaN"

NamedBitList ::=
    NamedBit
    <|> NamedBitList "," NamedBit

NamedBit ::=
    identifier "(" number ")"
    <|> identifier "(" DefinedValue ")"

XMLBitStringValue ::=
    XMLTypedValue
    <|> xmlbstring
    <|> XMLIdentifierList
    <|> empty

XMLIdentifierList ::=
    EmptyElementList
    <|> TextList

EmptyElementList ::=
    "<" & identifier "/>"
    <|> EmptyElementList "<" & identifier "/>"

TextList ::=
    identifier
    <|> TextList identifier

OctetStringType ::= OCTET STRING

OctetStringValue ::=
    bstring
    <|> hstring
    <|> CONTAINING Value

XMLOctetStringValue ::=
    XMLTypedValue
    <|> xmlhstring

NullType ::= NULL

NullValue ::= NULL

XMLNullValue ::= empty

SequenceType ::=
    SEQUENCE "{" "}"
    <|> SEQUENCE "{" ExtensionAndException OptionalExtensionMarker "}"
    <|> SEQUENCE "{" ComponentTypeLists "}"

OptionalExtensionMarker ::= "," "..." <|> empty

ComponentTypeLists ::=
    RootComponentTypeList
    <|> RootComponentTypeList "," ExtensionAndException ExtensionAdditions
    OptionalExtensionMarker
    <|> RootComponentTypeList "," ExtensionAndException ExtensionAdditions
    ExtensionEndMarker "," RootComponentTypeList
    <|> ExtensionAndException ExtensionAdditions ExensionEndMarker ","
    RootComponentTypeList
    <|> ExtensionAndException ExtensionAdditions OptionalExtensionMarker

RootComponentTypeList ::= ComponentTypeList

ExtensionEndMarker ::= "," "..."

ExtensionAdditions ::=
    "," ExtensionAdditionList
    <|> empty

ExtensionAdditionList ::=
    ExtensionAddition
    <|> ExtensionAdditionList "," ExtensionAddition

ExtensionAddition ::=
    ComponentType
    <|> ExtensionAdditionGroup

ExtensionAdditionGroup ::= "[[" VersionNumber ComponentTypeList "]]"

VersionNumber ::= empty <|> number ":"

ComponentTypeList ::=
    ComponentType
    <|> ComponentTypeList "," ComponentType

ComponentType ::=
    NamedType
    <|> NamedType OPTIONAL
    <|> NamedType DEFAULT Value
    <|> COMPONENTS OF Type

SequenceValue ::=
    "{" ComponentValueList "}"
    <|> "{" "}"

ComponentValueList ::=
    NamedValue
    <|> ComponentValueList "," NamedValue

XMLSequenceValue ::=
    XMLComponentValueList
    <|> empty

XMLComponentValueList ::=
    XMLNamedValue
    <|> XMLComponentValueList XMLNamedValue

SequenceOfType ::= SEQUENCE OF Type <|> SEQUENCE OF NamedType

SequenceOfValue ::=
    "{" ValueList "}"
    <|> "{" NamedValueList "}"
    <|> "{" "}"

ValueList ::=
    Value
    <|> ValueList "," Value

NamedValueList ::=
    NamedValue
    <|> NamedValueList "," NamedValue

XMLSequenceOfValue ::=
    XMLValueList
    <|> XMLDelimitedItemList
    <|> empty

XMLValueList ::=
    XMLValueOrEmpty
    <|> XMLValueOrEmpty XMLValueList

XMLValueOrEmpty ::=
    XMLValue
    <|> "<" & NonParameterizedTypeName "/>"

XMLDelimitedItemList ::=
    XMLDelimitedItem
    <|> XMLDelimitedItem XMLDelimitedItemList

XMLDelimitedItem ::=
    "<" & NonParameterizedTypeName ">" XMLValue
    "</" & NonParameterizedTypeName ">"
    <|> "<" & identifier ">" XMLValue "</" & identifier ">"

SetType ::=
    SET "{" "}"
    <|> SET "{" ExtensionAndException OptionalExtensionMarker "}"
    <|> SET "{" ComponentTypeLists "}"

SetValue ::=
    "{" ComponentValueList "}"
    <|> "{" "}"

XMLSetValue ::=
    XMLComponentValueList
    <|> empty

SetOfType ::=
    SET OF Type
    <|> SET OF NamedType

SetOfValue ::=
    "{" ValueList "}"
    <|> "{" NamedValueList "}"
    <|> "{" "}"

XMLSetOfValue ::=
    XMLValueList
    <|> XMLDelimitedItemList
    <|> empty

ChoiceValue ::= identifier ":" Value

XMLChoiceValue ::= "<" & identifier ">" XMLValue "</" & identifier ">"

SelectionType ::= identifier "<" Type

PrefixedType ::=
    TaggedType
    <|> EncodingPrefixedType

PrefixedValue ::= Value

XMLPrefixedValue ::= XMLValue

EncodingPrefixedType ::=
    EncodingPrefix Type

EncodingPrefix ::=
    "[" EncodingReference EncodingInstruction "]"

TaggedType ::=
    Tag Type
    <|> Tag IMPLICIT Type
    <|> Tag EXPLICIT Type

Tag ::= "[" EncodingReference Class ClassNumber "]"

EncodingReference ::=
    encodingreference ":"
    <|> empty

ClassNumber ::=
    number
    <|> DefinedValue

Class ::=
    UNIVERSAL
    <|> APPLICATION
    <|> PRIVATE
    <|> empty

EncodingPrefixedType ::=
    EncodingPrefix Type

EncodingPrefix ::=
    "[" EncodingReference EncodingInstruction "]"

ObjectIdentifierType ::=
    OBJECT IDENTIFIER

XMLObjectIdentifierValue ::=
    XMLObjIdComponentList

XMLObjIdComponentList ::=
    XMLObjIdComponent
    <|> XMLObjIdComponent & "." & XMLObjIdComponentList

XMLObjIdComponent ::=
    NameForm
    <|> XMLNumberForm
    <|> XMLNameAndNumberForm

XMLNumberForm ::= number

XMLNameAndNumberForm ::=
    identifier & "(" & XMLNumberForm & ")"

RelativeOIDType ::= RELATIVE-OID

RelativeOIDValue ::=
    "{" RelativeOIDComponentsList "}"

RelativeOIDComponentsList ::=
    RelativeOIDComponents
    <|> RelativeOIDComponents RelativeOIDComponentsList

RelativeOIDComponents ::=
    NumberForm
    <|> NameAndNumberForm
    <|> DefinedValue

XMLRelativeOIDValue ::=
    XMLRelativeOIDComponentList

XMLRelativeOIDComponentList ::=
    XMLRelativeOIDComponent
    <|> XMLRelativeOIDComponent & "." & XMLRelativeOIDComponentList

XMLRelativeOIDComponent ::=
    XMLNumberForm
    <|> XMLNameAndNumberForm

IRIType ::= OID-IRI

FirstArcIdentifier ::=
    "/" ArcIdentifier

SubsequentArcIdentifier ::=
    "/" ArcIdentifier SubsequentArcIdentifier
    <|> empty

ArcIdentifier ::=
    integerUnicodeLabel
    <|> non-integerUnicodeLabel

XMLIRIValue ::=
    FirstArcIdentifier
    SubsequentArcIdentifier

RelativeIRIType ::= RELATIVE-OID-IRI

RelativeIRIValue ::=
    """
    FirstRelativeArcIdentifier
    SubsequentArcIdentifier
    """

FirstRelativeArcIdentifier ::=
    ArcIdentifier

XMLRelativeIRIValue ::=
    FirstRelativeArcIdentifier
    SubsequentArcIdentifier

EmbeddedPDVType ::= EMBEDDED PDV

EmbeddedPDVValue ::= SequenceValue

XMLEmbeddedPDVValue ::= XMLSequenceValue

ExternalType ::= EXTERNAL

ExternalValue ::= SequenceValue

XMLExternalValue ::= XMLSequenceValue

TimeType ::= TIME

TimeValue ::= tstring

XMLTimeValue ::= xmltstring

DateType ::= DATE

TimeOfDayType ::= TIME-OF-DAY

DateTimeType ::= DATE-TIME

DurationType ::= DURATION

XMLCharacterStringValue ::=
    XMLRestrictedCharacterStringValue
    <|> XMLUnrestrictedCharacterStringValue

CharacterStringList ::= "{" CharSyms "}"

CharSyms ::=
    CharsDefn
    <|> CharSyms "," CharsDefn

CharsDefn ::=
    cstring
    <|> Quadruple
    <|> Tuple
    <|> DefinedValue

Quadruple ::= "{" Group "," Plane "," Row "," Cell "}"

Group ::= number

Plane ::= number

Row ::= number

Cell ::= number

Tuple ::= "{" TableColumn "," TableRow "}"

TableColumn ::= number

TableRow ::= number

XMLRestrictedCharacterStringValue ::= xmlcstring

XMLUnrestrictedCharacterStringValue ::= XMLSequenceValue

UsefulType ::= typereference
    The following character string types are defined in 41.1:
    UTF8String GraphicString
    NumericString VisibleString
    PrintableString ISO646String
    TeletexString GeneralString
    T61String UniversalString
    VideotexString BMPString
    IA5String
    The following useful types are defined in clauses 46 to 48:
    GeneralizedTime
    UTCTime
    ObjectDescriptor
    The following productions are used in clauses 49 to 51:

ConstrainedType ::=
    Type Constraint
    <|> TypeWithConstraint

TypeWithConstraint ::=
    SET Constraint OF Type
    <|> SET SizeConstraint OF Type
    <|> SEQUENCE Constraint OF Type
    <|> SEQUENCE SizeConstraint OF Type
    <|> SET Constraint OF NamedType
    <|> SET SizeConstraint OF NamedType
    <|> SEQUENCE Constraint OF NamedType
    <|> SEQUENCE SizeConstraint OF NamedType

Constraint ::= "(" ConstraintSpec ExceptionSpec ")"

ConstraintSpec ::= SubtypeConstraint
    <|> GeneralConstraint

SubtypeConstraint ::= ElementSetSpecs

ElementSetSpecs ::=
    RootElementSetSpec
    <|> RootElementSetSpec "," "..."
    <|> RootElementSetSpec "," "..." "," AdditionalElementSetSpec

RootElementSetSpec ::= ElementSetSpec

AdditionalElementSetSpec ::= ElementSetSpec

ElementSetSpec ::= Unions
    <|> ALL Exclusions

Unions ::= Intersections
    <|> UElems UnionMark Intersections

UElems ::= Unions

Intersections ::= IntersectionElements
    <|> IElems IntersectionMark IntersectionElements

IElems ::= Intersections

IntersectionElements ::= Elements <|> Elems Exclusions

Elems ::= Elements

Exclusions ::= EXCEPT Elements

UnionMark ::= "<|>" <|> UNION

IntersectionMark ::= "^" <|> INTERSECTION

Elements ::=
    SubtypeElements
    <|> ObjectSetElements
    <|> "(" ElementSetSpec ")"

SubtypeElements ::=
    SingleValue
    <|> ContainedSubtype
    <|> ValueRange
    <|> PermittedAlphabet
    <|> SizeConstraint
    <|> TypeConstraint
    <|> InnerTypeConstraints
    <|> PatternConstraint
    <|> PropertySettings
    <|> DurationRange
    <|> TimePointRange
    <|> RecurrenceRange

SingleValue ::= Value

ContainedSubtype ::= Includes Type

Includes ::= INCLUDES <|> empty

ValueRange ::= LowerEndpoint ".." UpperEndpoint

LowerEndpoint ::= LowerEndValue <|> LowerEndValue "<"

UpperEndpoint ::= UpperEndValue <|> "<" UpperEndValue

LowerEndValue ::= Value <|> MIN

UpperEndValue ::= Value <|> MAX

SizeConstraint ::= SIZE Constraint

TypeConstraint ::= Type

PermittedAlphabet ::= FROM Constraint

InnerTypeConstraints ::=
    WITH COMPONENT SingleTypeConstraint
    <|> WITH COMPONENTS MultipleTypeConstraints
SingleTypeConstraint::= Constraint

MultipleTypeConstraints ::=
    FullSpecification
    <|> PartialSpecification

FullSpecification ::= "{" TypeConstraints "}"

PartialSpecification ::= "{" "..." "," TypeConstraints "}"

TypeConstraints ::=
    NamedConstraint
    <|> NamedConstraint "," TypeConstraints

NamedConstraint ::=
    identifier ComponentConstraint

ComponentConstraint ::= ValueConstraint PresenceConstraint

ValueConstraint ::= Constraint <|> empty

PresenceConstraint ::= PRESENT <|> ABSENT <|> OPTIONAL <|> empty

PatternConstraint ::= PATTERN Value

PropertySettings ::= SETTINGS simplestring

PropertySettingsList ::=
    PropertyAndSettingPair
    <|> PropertySettingsList PropertyAndSettingPair

PropertyAndSettingPair ::= PropertyName "=" SettingName

PropertyName ::= psname

SettingName ::= psname

DurationRange ::= ValueRange

TimePointRange ::= ValueRange

RecurrenceRange ::= ValueRange
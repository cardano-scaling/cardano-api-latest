module Cardano.Api.Latest where

import Cardano.Api qualified
import Cardano.Ledger.Alonzo.TxAuxData qualified as Ledger
import Cardano.Ledger.Alonzo.TxWits qualified as Ledger
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Keys qualified as Ledger
import Data.ByteString.Short (ShortByteString)
import Data.Map (Map)

type Era = Cardano.Api.ConwayEra

type LedgerEra = Cardano.Api.ShelleyLedgerEra Era

-- ** AddressInEra

type AddressInEra = Cardano.Api.AddressInEra Era
{-# COMPLETE ShelleyAddressInEra, ByronAddressInEra #-}

pattern ShelleyAddressInEra :: Cardano.Api.Address Cardano.Api.ShelleyAddr -> AddressInEra
pattern ShelleyAddressInEra{address} <-
    Cardano.Api.AddressInEra Cardano.Api.ShelleyAddressInEra{} address
    where
        ShelleyAddressInEra =
            Cardano.Api.AddressInEra ShelleyAddressInAnyEra

pattern ByronAddressInEra :: Cardano.Api.Address Cardano.Api.ByronAddr -> AddressInEra
pattern ByronAddressInEra{byronAddress} <-
    Cardano.Api.AddressInEra Cardano.Api.ByronAddressInAnyEra byronAddress
    where
        ByronAddressInEra =
            Cardano.Api.AddressInEra ByronAddressInAnyEra

-- ** AddressTypeInEra

type AddressTypeInEra addrType = Cardano.Api.AddressTypeInEra addrType Era
{-# COMPLETE ByronAddressInAnyEra, ShelleyAddressInAnyEra #-}

pattern ByronAddressInAnyEra :: AddressTypeInEra Cardano.Api.ByronAddr
pattern ByronAddressInAnyEra <-
    Cardano.Api.ByronAddressInAnyEra
    where
        ByronAddressInAnyEra =
            Cardano.Api.ByronAddressInAnyEra

pattern ShelleyAddressInAnyEra :: AddressTypeInEra Cardano.Api.ShelleyAddr
pattern ShelleyAddressInAnyEra <-
    Cardano.Api.ShelleyAddressInEra _
    where
        ShelleyAddressInAnyEra =
            Cardano.Api.ShelleyAddressInEra Cardano.Api.shelleyBasedEra

-- ** BalancedTxBody

type BalancedTxBody = Cardano.Api.BalancedTxBody Era
{-# COMPLETE BalancedTxBody #-}

pattern BalancedTxBody :: TxBodyContent Cardano.Api.BuildTx -> TxBody -> TxOut Cardano.Api.CtxTx -> Cardano.Api.Coin -> BalancedTxBody
pattern BalancedTxBody{balancedTxBodyContent, balancedTxBody, balancedTxChangeOutput, balancedTxFee} <-
    Cardano.Api.BalancedTxBody balancedTxBodyContent balancedTxBody balancedTxChangeOutput balancedTxFee
    where
        BalancedTxBody =
            Cardano.Api.BalancedTxBody

-- ** KeyWitness

type KeyWitness = Cardano.Api.KeyWitness Era
{-# COMPLETE ShelleyBootstrapWitness, ShelleyKeyWitness #-}

pattern ShelleyBootstrapWitness :: Ledger.BootstrapWitness -> KeyWitness
pattern ShelleyBootstrapWitness{shelleyBootstrapWitness} <-
    Cardano.Api.ShelleyBootstrapWitness _ shelleyBootstrapWitness
    where
        ShelleyBootstrapWitness =
            Cardano.Api.ShelleyBootstrapWitness Cardano.Api.shelleyBasedEra

pattern ShelleyKeyWitness :: Ledger.WitVKey 'Ledger.Witness -> KeyWitness
pattern ShelleyKeyWitness{shelleyKeyWitness} <-
    Cardano.Api.ShelleyKeyWitness _ shelleyKeyWitness
    where
        ShelleyKeyWitness =
            Cardano.Api.ShelleyKeyWitness Cardano.Api.shelleyBasedEra

-- ** PlutusScript

type PlutusScript = Cardano.Api.PlutusScript Cardano.Api.PlutusScriptV3
{-# COMPLETE PlutusScriptSerialised #-}

pattern PlutusScriptSerialised :: ShortByteString -> PlutusScript
pattern PlutusScriptSerialised{plutusScriptSerialised} <-
    Cardano.Api.PlutusScriptSerialised plutusScriptSerialised
    where
        PlutusScriptSerialised =
            Cardano.Api.PlutusScriptSerialised

-- ** Script

type Script = Cardano.Api.Script Cardano.Api.PlutusScriptV3
{-# COMPLETE PlutusScript #-}

pattern PlutusScript :: PlutusScript -> Script
pattern PlutusScript{plutusScript} <-
    Cardano.Api.PlutusScript _ plutusScript
    where
        PlutusScript =
            Cardano.Api.PlutusScript Cardano.Api.PlutusScriptV3

-- ** ScriptInEra

type ScriptInEra = Cardano.Api.ScriptInEra Era

-- ** ScriptLanguage

type ScriptLanguage = Cardano.Api.ScriptLanguage Cardano.Api.PlutusScriptV3
{-# COMPLETE PlutusScriptLanguage #-}

pattern PlutusScriptLanguage :: ScriptLanguage
pattern PlutusScriptLanguage <-
    Cardano.Api.PlutusScriptLanguage _
    where
        PlutusScriptLanguage =
            Cardano.Api.PlutusScriptLanguage Cardano.Api.PlutusScriptV3

-- ** ScriptWitness

type ScriptWitness witCtx = Cardano.Api.ScriptWitness witCtx Era
{-# COMPLETE PlutusScriptWitness #-}

pattern PlutusScriptWitness ::
    PlutusScript ->
    Cardano.Api.ScriptDatum witctx ->
    Cardano.Api.ScriptRedeemer ->
    Cardano.Api.ExecutionUnits ->
    ScriptWitness witctx
pattern PlutusScriptWitness
    { plutusScriptWitnessScript
    , plutusScriptWitnessDatum
    , plutusScriptWitnessRedeemer
    , plutusScriptWitnessExecutionUnits
    } <-
    Cardano.Api.PlutusScriptWitness
        _
        Cardano.Api.PlutusScriptV3
        (Cardano.Api.PScript plutusScriptWitnessScript)
        plutusScriptWitnessDatum
        plutusScriptWitnessRedeemer
        plutusScriptWitnessExecutionUnits
    where
        PlutusScriptWitness =
            Cardano.Api.PlutusScriptWitness
                Cardano.Api.scriptLanguageInEra
                Cardano.Api.PlutusScriptV3
                . Cardano.Api.PScript

-- ** Tx

type Tx = Cardano.Api.Tx Era
{-# COMPLETE Tx #-}
{-# COMPLETE ShelleyTxBody #-}

pattern Tx :: TxBody -> [KeyWitness] -> Tx
pattern Tx{txBody, txKeyWitnesses} <-
    Cardano.Api.Tx txBody txKeyWitnesses
    where
        Tx =
            Cardano.Api.Tx

pattern ShelleyTxBody ::
    Ledger.TxBody LedgerEra ->
    [Ledger.Script LedgerEra] ->
    TxBodyScriptData ->
    Maybe (Ledger.AlonzoTxAuxData LedgerEra) ->
    TxScriptValidity ->
    TxBody
pattern ShelleyTxBody
    { txBodyLedgerTxBody
    , txBodyScripts
    , txBodyScriptData
    , txBodyAuxiliaryData
    , txBodyScriptValidity
    } <-
    Cardano.Api.ShelleyTxBody
        _
        txBodyLedgerTxBody
        txBodyScripts
        txBodyScriptData
        txBodyAuxiliaryData
        txBodyScriptValidity
    where
        ShelleyTxBody =
            Cardano.Api.ShelleyTxBody Cardano.Api.shelleyBasedEra

signShelleyTransaction :: TxBody -> [Cardano.Api.ShelleyWitnessSigningKey] -> Tx
signShelleyTransaction = Cardano.Api.signShelleyTransaction Cardano.Api.shelleyBasedEra

-- ** TxAuxScripts

type TxAuxScripts = Cardano.Api.TxAuxScripts Era
{-# COMPLETE TxAuxScriptsNone, TxAuxScripts #-}

pattern TxAuxScriptsNone :: TxAuxScripts
pattern TxAuxScriptsNone <-
    Cardano.Api.TxAuxScriptsNone
    where
        TxAuxScriptsNone =
            Cardano.Api.TxAuxScriptsNone

pattern TxAuxScripts :: [ScriptInEra] -> TxAuxScripts
pattern TxAuxScripts{txAuxScripts'} <-
    Cardano.Api.TxAuxScripts _ txAuxScripts'
    where
        TxAuxScripts =
            Cardano.Api.TxAuxScripts Cardano.Api.allegraBasedEra

-- ** TxBody

type TxBody = Cardano.Api.TxBody Era

createAndValidateTransactionBody :: TxBodyContent Cardano.Api.BuildTx -> Either Cardano.Api.TxBodyError TxBody
createAndValidateTransactionBody = Cardano.Api.createTransactionBody Cardano.Api.shelleyBasedEra

defaultTxBodyContent :: TxBodyContent Cardano.Api.BuildTx
defaultTxBodyContent = Cardano.Api.defaultTxBodyContent Cardano.Api.shelleyBasedEra

-- ** TxBodyContent

type TxBodyContent build = Cardano.Api.TxBodyContent build Era
{-# COMPLETE TxBodyContent #-}

pattern TxBodyContent ::
    TxIns build ->
    TxInsCollateral ->
    TxInsReference build ->
    [TxOut Cardano.Api.CtxTx] ->
    Cardano.Api.TxTotalCollateral Era ->
    Cardano.Api.TxReturnCollateral Cardano.Api.CtxTx Era ->
    TxFee ->
    TxValidityLowerBound ->
    TxValidityUpperBound ->
    TxMetadataInEra ->
    TxAuxScripts ->
    TxExtraKeyWitnesses ->
    Cardano.Api.BuildTxWith build (Maybe (Cardano.Api.LedgerProtocolParameters Era)) ->
    Cardano.Api.TxWithdrawals build Era ->
    Cardano.Api.TxCertificates build Era ->
    Cardano.Api.TxUpdateProposal Era ->
    TxMintValue build ->
    TxScriptValidity ->
    Maybe (Cardano.Api.Featured Cardano.Api.ConwayEraOnwards Era (Cardano.Api.TxProposalProcedures build Era)) ->
    Maybe (Cardano.Api.Featured Cardano.Api.ConwayEraOnwards Era (Cardano.Api.TxVotingProcedures build Era)) ->
    Maybe (Cardano.Api.Featured Cardano.Api.ConwayEraOnwards Era (Maybe Cardano.Api.Coin)) ->
    Maybe (Cardano.Api.Featured Cardano.Api.ConwayEraOnwards Era Cardano.Api.Coin) ->
    TxBodyContent build
pattern TxBodyContent
    { txIns
    , txInsCollateral
    , txInsReference
    , txOuts
    , txTotalCollateral
    , txReturnCollateral
    , txFee
    , txValidityLowerBound
    , txValidityUpperBound
    , txMetadata
    , txAuxScripts
    , txExtraKeyWits
    , txProtocolParams
    , txWithdrawals
    , txCertificates
    , txUpdateProposal
    , txMintValue
    , txScriptValidity
    , txProposalProcedures
    , txVotingProcedures
    , txCurrentTreasuryValue
    , txTreasuryDonation
    } <-
    Cardano.Api.TxBodyContent
        txIns
        txInsCollateral
        txInsReference
        txOuts
        txTotalCollateral
        txReturnCollateral
        txFee
        txValidityLowerBound
        txValidityUpperBound
        txMetadata
        txAuxScripts
        txExtraKeyWits
        txProtocolParams
        txWithdrawals
        txCertificates
        txUpdateProposal
        txMintValue
        txScriptValidity
        txProposalProcedures
        txVotingProcedures
        txCurrentTreasuryValue
        txTreasuryDonation
    where
        TxBodyContent = Cardano.Api.TxBodyContent

-- ** TxBodyScriptData

type TxBodyScriptData = Cardano.Api.TxBodyScriptData Era
{-# COMPLETE TxBodyNoScriptData, TxBodyScriptData #-}

pattern TxBodyNoScriptData :: TxBodyScriptData
pattern TxBodyNoScriptData <-
    Cardano.Api.TxBodyNoScriptData
    where
        TxBodyNoScriptData =
            Cardano.Api.TxBodyNoScriptData

pattern TxBodyScriptData ::
    Ledger.TxDats (Cardano.Api.ShelleyLedgerEra Era) ->
    Ledger.Redeemers (Cardano.Api.ShelleyLedgerEra Era) ->
    TxBodyScriptData
pattern TxBodyScriptData{txBodyScriptDatums, txBodyScriptRedeemers} <-
    Cardano.Api.TxBodyScriptData _ txBodyScriptDatums txBodyScriptRedeemers
    where
        TxBodyScriptData =
            Cardano.Api.TxBodyScriptData Cardano.Api.alonzoBasedEra

-- ** TxExtraKeyWitnesses

type TxExtraKeyWitnesses = Cardano.Api.TxExtraKeyWitnesses Era
{-# COMPLETE TxExtraKeyWitnessesNone, TxExtraKeyWitnesses #-}

pattern TxExtraKeyWitnessesNone :: TxExtraKeyWitnesses
pattern TxExtraKeyWitnessesNone <-
    Cardano.Api.TxExtraKeyWitnessesNone
    where
        TxExtraKeyWitnessesNone = Cardano.Api.TxExtraKeyWitnessesNone

pattern TxExtraKeyWitnesses :: [Cardano.Api.Hash Cardano.Api.PaymentKey] -> TxExtraKeyWitnesses
pattern TxExtraKeyWitnesses{txExtraKeyWitnesses} <-
    Cardano.Api.TxExtraKeyWitnesses _ txExtraKeyWitnesses
    where
        TxExtraKeyWitnesses =
            Cardano.Api.TxExtraKeyWitnesses Cardano.Api.alonzoBasedEra

-- ** TxFee

type TxFee = Cardano.Api.TxFee Era
{-# COMPLETE TxFeeExplicit #-}

pattern TxFeeExplicit :: Cardano.Api.Coin -> TxFee
pattern TxFeeExplicit{txFeeExplicit} <-
    Cardano.Api.TxFeeExplicit _ txFeeExplicit
    where
        TxFeeExplicit =
            Cardano.Api.TxFeeExplicit Cardano.Api.shelleyBasedEra

-- ** TxIns

type TxIns build = [(Cardano.Api.TxIn, Cardano.Api.BuildTxWith build (Cardano.Api.Witness Cardano.Api.WitCtxTxIn Era))]

-- ** TxInsReference

type TxInsReference build = Cardano.Api.TxInsReference build Era
{-# COMPLETE TxInsReferenceNone, TxInsReference #-}

pattern TxInsReferenceNone :: TxInsReference build
pattern TxInsReferenceNone <-
    Cardano.Api.TxInsReferenceNone
    where
        TxInsReferenceNone =
            Cardano.Api.TxInsReferenceNone

pattern TxInsReference :: [Cardano.Api.TxIn] -> Cardano.Api.TxInsReferenceDatums build -> TxInsReference build
pattern TxInsReference{txInsReference', scriptData} <-
    Cardano.Api.TxInsReference _ txInsReference' scriptData
    where
        TxInsReference =
            Cardano.Api.TxInsReference Cardano.Api.babbageBasedEra

-- ** TxInsCollateral

type TxInsCollateral = Cardano.Api.TxInsCollateral Era
{-# COMPLETE TxInsCollateralNone, TxInsCollateral #-}

pattern TxInsCollateralNone :: TxInsCollateral
pattern TxInsCollateralNone <-
    Cardano.Api.TxInsCollateralNone
    where
        TxInsCollateralNone =
            Cardano.Api.TxInsCollateralNone

pattern TxInsCollateral :: [Cardano.Api.TxIn] -> TxInsCollateral
pattern TxInsCollateral{txInsCollateral'} <-
    Cardano.Api.TxInsCollateral _ txInsCollateral'
    where
        TxInsCollateral =
            Cardano.Api.TxInsCollateral Cardano.Api.alonzoBasedEra

-- ** TxMetadataInEra

type TxMetadataInEra = Cardano.Api.TxMetadataInEra Era
{-# COMPLETE TxMetadataNone, TxMetadataInEra #-}

pattern TxMetadataNone :: TxMetadataInEra
pattern TxMetadataNone <-
    Cardano.Api.TxMetadataNone
    where
        TxMetadataNone =
            Cardano.Api.TxMetadataNone

pattern TxMetadataInEra :: Cardano.Api.TxMetadata -> TxMetadataInEra
pattern TxMetadataInEra{txMetadataInEra} <-
    Cardano.Api.TxMetadataInEra _ txMetadataInEra
    where
        TxMetadataInEra =
            Cardano.Api.TxMetadataInEra Cardano.Api.shelleyBasedEra

-- ** TxMintValue

type TxMintValue build = Cardano.Api.TxMintValue build Era
{-# COMPLETE TxMintValueNone, TxMintValue #-}

pattern TxMintValueNone :: TxMintValue build
pattern TxMintValueNone <-
    Cardano.Api.TxMintNone
    where
        TxMintValueNone =
            Cardano.Api.TxMintNone

pattern TxMintValue ::
    Map Cardano.Api.PolicyId (Cardano.Api.PolicyAssets, Cardano.Api.BuildTxWith build (ScriptWitness Cardano.Api.WitCtxMint)) ->
    TxMintValue build
pattern TxMintValue{txMintValueInEra} <-
    Cardano.Api.TxMintValue _ txMintValueInEra
    where
        TxMintValue =
            Cardano.Api.TxMintValue Cardano.Api.maryBasedEra
-- ** TxOut

type TxOut ctx = Cardano.Api.TxOut ctx Era
{-# COMPLETE TxOut #-}

-- | TxOut specialized for 'Era'
pattern TxOut :: AddressInEra -> Cardano.Api.Value -> TxOutDatum ctx -> ReferenceScript -> TxOut ctx
pattern TxOut{txOutAddress, txOutValue, txOutDatum, txOutReferenceScript} <-
    Cardano.Api.TxOut
        txOutAddress
        (Cardano.Api.TxOutValueShelleyBased _ (Cardano.Api.fromMaryValue -> txOutValue))
        txOutDatum
        txOutReferenceScript
    where
        TxOut addr value datum ref =
            Cardano.Api.TxOut
                addr
                ( Cardano.Api.TxOutValueShelleyBased
                    Cardano.Api.shelleyBasedEra
                    (Cardano.Api.toMaryValue value)
                )
                datum
                ref

-- ** ReferenceScript

type ReferenceScript = Cardano.Api.ReferenceScript Era
{-# COMPLETE ReferenceScript, ReferenceScriptNone #-}

pattern ReferenceScript :: Cardano.Api.ScriptInAnyLang -> ReferenceScript
pattern ReferenceScript{referenceScript} <-
    Cardano.Api.ReferenceScript
        _
        referenceScript
    where
        ReferenceScript =
            Cardano.Api.ReferenceScript
                Cardano.Api.babbageBasedEra

pattern ReferenceScriptNone :: Cardano.Api.ReferenceScript Era
pattern ReferenceScriptNone <-
    Cardano.Api.ReferenceScriptNone
    where
        ReferenceScriptNone =
            Cardano.Api.ReferenceScriptNone

-- ** TxOutDatum

type TxOutDatum ctx = Cardano.Api.TxOutDatum ctx Era
{-# COMPLETE TxOutDatumNone, TxOutDatumHash, TxOutSupplementalDatum, TxOutDatumInline #-}

pattern TxOutDatumNone :: TxOutDatum ctx
pattern TxOutDatumNone <-
    Cardano.Api.TxOutDatumNone
    where
        TxOutDatumNone =
            Cardano.Api.TxOutDatumNone

pattern TxOutDatumHash :: Cardano.Api.Hash Cardano.Api.ScriptData -> TxOutDatum ctx
pattern TxOutDatumHash{txOutDatumHash} <-
    Cardano.Api.TxOutDatumHash _ txOutDatumHash
    where
        TxOutDatumHash =
            Cardano.Api.TxOutDatumHash Cardano.Api.alonzoBasedEra

pattern TxOutSupplementalDatum :: Cardano.Api.HashableScriptData -> TxOutDatum Cardano.Api.CtxTx
pattern TxOutSupplementalDatum{txOutDatumScriptData} <-
    Cardano.Api.TxOutSupplementalDatum _ txOutDatumScriptData
    where
        TxOutSupplementalDatum =
            Cardano.Api.TxOutSupplementalDatum Cardano.Api.alonzoBasedEra

pattern TxOutDatumInline :: Cardano.Api.HashableScriptData -> TxOutDatum ctx
pattern TxOutDatumInline{txOutDatumInlineScriptData} <-
    Cardano.Api.TxOutDatumInline _ txOutDatumInlineScriptData
    where
        TxOutDatumInline =
            Cardano.Api.TxOutDatumInline Cardano.Api.babbageBasedEra

-- ** TxScriptValidity

type TxScriptValidity = Cardano.Api.TxScriptValidity Era
{-# COMPLETE TxScriptValidityNone, TxScriptValidity #-}

pattern TxScriptValidityNone :: TxScriptValidity
pattern TxScriptValidityNone <-
    Cardano.Api.TxScriptValidityNone
    where
        TxScriptValidityNone =
            Cardano.Api.TxScriptValidityNone

pattern TxScriptValidity :: Cardano.Api.ScriptValidity -> TxScriptValidity
pattern TxScriptValidity{txScriptValidity'} <-
    Cardano.Api.TxScriptValidity _ txScriptValidity'
    where
        TxScriptValidity =
            Cardano.Api.TxScriptValidity Cardano.Api.alonzoBasedEra

-- ** TxValidityLowerBound

type TxValidityLowerBound = Cardano.Api.TxValidityLowerBound Era
{-# COMPLETE TxValidityNoLowerBound, TxValidityLowerBound #-}

pattern TxValidityNoLowerBound :: TxValidityLowerBound
pattern TxValidityNoLowerBound <-
    Cardano.Api.TxValidityNoLowerBound
    where
        TxValidityNoLowerBound =
            Cardano.Api.TxValidityNoLowerBound

pattern TxValidityLowerBound :: Cardano.Api.SlotNo -> TxValidityLowerBound
pattern TxValidityLowerBound{lowerBound} <-
    Cardano.Api.TxValidityLowerBound _ lowerBound
    where
        TxValidityLowerBound =
            Cardano.Api.TxValidityLowerBound Cardano.Api.allegraBasedEra

-- ** TxValidityUpperBound

type TxValidityUpperBound = Cardano.Api.TxValidityUpperBound Era
{-# COMPLETE TxValidityNoUpperBound, TxValidityUpperBound #-}

pattern TxValidityNoUpperBound :: TxValidityUpperBound
pattern TxValidityNoUpperBound <-
    Cardano.Api.TxValidityUpperBound _ Nothing
    where
        TxValidityNoUpperBound =
            Cardano.Api.TxValidityUpperBound
                Cardano.Api.shelleyBasedEra
                Nothing

pattern TxValidityUpperBound :: Cardano.Api.SlotNo -> TxValidityUpperBound
pattern TxValidityUpperBound{upperBound} <-
    Cardano.Api.TxValidityUpperBound _ (Just upperBound)
    where
        TxValidityUpperBound =
            Cardano.Api.TxValidityUpperBound Cardano.Api.shelleyBasedEra
                . Just

-- ** Witness

type Witness witCtx = Cardano.Api.Witness witCtx Era
{-# COMPLETE ScriptWitness, KeyWitness #-}

pattern KeyWitness :: Cardano.Api.KeyWitnessInCtx ctx -> Witness ctx
pattern KeyWitness keyWitnessInCtx <-
    Cardano.Api.KeyWitness keyWitnessInCtx
    where
        KeyWitness = Cardano.Api.KeyWitness

pattern ScriptWitness :: Cardano.Api.ScriptWitnessInCtx ctx -> ScriptWitness ctx -> Witness ctx
pattern ScriptWitness scriptWitnessInCtx scriptWitness <-
    Cardano.Api.ScriptWitness scriptWitnessInCtx scriptWitness
    where
        ScriptWitness = Cardano.Api.ScriptWitness

makeShelleyKeyWitness :: TxBody -> Cardano.Api.ShelleyWitnessSigningKey -> KeyWitness
makeShelleyKeyWitness = Cardano.Api.makeShelleyKeyWitness Cardano.Api.shelleyBasedEra

type UTxO = Cardano.Api.UTxO Era

{-# COMPLETE UTxO #-}
pattern UTxO ::
    Map Cardano.Api.TxIn (TxOut Cardano.Api.CtxUTxO) ->
    UTxO
pattern UTxO{utxo} <-
    Cardano.Api.UTxO utxo
    where
        UTxO =
            Cardano.Api.UTxO

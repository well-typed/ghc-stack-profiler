{-# LANGUAGE OverloadedRecordDot #-}

module GHC.Stack.Profiler.Speedscope.IpeDb (toSpeedscopeInfoProv) where

import qualified GHC.Stack.Profiler.Speedscope.Types as Speedscope
import qualified IpeDb.InfoProv as IpeDb

toSpeedscopeInfoProv :: IpeDb.InfoProv -> Speedscope.InfoProv
toSpeedscopeInfoProv ipedb_info_prov =
  Speedscope.InfoProv
    { Speedscope.infoProvId = Speedscope.InfoProvId ipedb_info_prov.infoId.id
    , Speedscope.infoProvSrcLoc = ipedb_info_prov.srcLoc
    , Speedscope.infoProvModule = ipedb_info_prov.moduleName
    , Speedscope.infoProvLabel = ipedb_info_prov.label
    , Speedscope.infoTableName = ipedb_info_prov.tableName
    , Speedscope.infoClosureDesc = fromIntegral ipedb_info_prov.closureDesc
    , Speedscope.infoTyDesc = ipedb_info_prov.typeDesc
    }

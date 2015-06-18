-module(geonames_log).

-export([msg/2, warn/2, error/2]).


msg(Fmt, Args ) -> error_logger:info_msg("[geonames] " ++ Fmt ++ "\n", Args).
warn(Fmt, Args ) -> error_logger:warning_msg("[geonames] " ++ Fmt ++ "\n", Args).
error(Fmt, Args ) -> error_logger:error_msg("[geonames] " ++ Fmt ++ "\n", Args).

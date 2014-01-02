%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%% @copyright (C) 2013, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created : 30 Dec 2013 by HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(bulletio_crypto).

%% API
-export([ivec/0, init/2, encrypt/2, decrypt/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc create IVector.
%% @end
%%--------------------------------------------------------------------
ivec() ->
    crypto:rand_bytes(16). %% 128 bit

%%--------------------------------------------------------------------
%% @doc init crypto state.
%% @end
%%--------------------------------------------------------------------
-spec init(AESKey, IVec) -> State when
      AESKey :: iodata(),
      IVec :: binary(),
      State :: crypto:opaque().
init(AESKey, IVec) when byte_size(AESKey) =:= 16,   %% 128 bit
			byte_size(IVec)   =:= 16 -> %% 128 bit
    crypto:stream_init(aes_ctr, AESKey, IVec).

%%--------------------------------------------------------------------
%% @doc encrypto palintext -> chipertext using AES Counter mode.
%% @end
%%--------------------------------------------------------------------
-spec encrypt(PlainText, State) -> {NewState, ChiperText} when
      PlainText :: iodata(),
      State :: crypto:opaque(),
      NewState :: crypto:opaque(),
      ChiperText :: binary().
encrypt(PlainText, State) ->
    crypto:stream_encrypt(State, PlainText).

%%--------------------------------------------------------------------
%% @doc decrypto  chipertext -> palintext using AES Counter mode.
%% @end
%%--------------------------------------------------------------------
-spec decrypt(ChiperText, State) -> {NewState, PlainText} when
      ChiperText :: iodata(),
      State :: crypto:opaque(),
      NewState :: cryoto:opaque(),
      PlainText :: binary().
decrypt(ChiperText, State) ->
    crypto:stream_decrypt(State, ChiperText).

%%%===================================================================
%%% Internal functions
%%%===================================================================

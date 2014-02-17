-record(socket_info, {
	type :: server | client,
	ip :: binary(),
	port :: integer(),
  name :: atom()
}).


-record(consumer_info, {
  name :: atom(),
  pid :: pid()
}).



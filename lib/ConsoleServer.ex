defmodule ConsoleServer do
  use GenServer.Behaviour

  defmodule State do
    defstruct port: nil, bindings: []
  end

  # Public API
  def start_link do
    :gen_server.start_link({:local, __MODULE__}, __MODULE__, [], [])
  end

  def stop do
    :gen_server.cast __MODULE__, :stop
  end

  def set_url(url) do
    :gen_server.cast __MODULE__, {:set_url, url}
  end

  def goto_home do
    set_url(home())
  end

  # gen_server callbacks
  def init(_args) do
    executable = :code.priv_dir(:pljelixir) ++ '/console'
    port = Port.open({:spawn_executable, executable},
    [{:packet, 2}, :use_stdio, :binary])
    state = %State{port: port}
    { :ok, state }
  end

  def handle_cast({:set_url, url}, state) do
    cast_port(state, :set_url, [url])
    {:noreply, state}
  end
  def handle_cast(:stop, state) do
    {:stop, :normal, state}
  end

  def handle_info({_, {:data, message}}, state) do
    msg = :erlang.binary_to_term(message)
    IO.puts "Unexpected message: " <> inspect(msg)
    {:noreply, state}
  end

  # Private helper functions
  defp cast_port(state, command, arguments) do
    msg = {command, arguments}
    send state.port, {self, {:command, :erlang.term_to_binary(msg)}}
  end

  defp call_port(state, command, arguments) do
    cast_port(state, command, arguments)
    receive do
      {_, {:data, response}} ->
        {:ok, :erlang.binary_to_term(response)}
        _ -> :error
    end
  end

  defp home do
    list_to_bitstring('file://' ++ :code.priv_dir(:pljelixir) ++ '/html/index.html')
  end
end

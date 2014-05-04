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

  # gen_server callbacks
  def init(_args) do
    executable = :code.priv_dir(:console) ++ '/console'
    port = Port.open({:spawn_executable, executable},
    [{:packet, 2}, :use_stdio, :binary])
    state = %State{port: port}
    cast_port(state, :prompt, [])
    { :ok, state }
  end

  def handle_call(:ping, _from, state) do
    {:ok, response} = call_port(state, :output, ["hello"])
    {:reply, response, state }
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
    handle_port(msg, state)
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

  defp handle_port({:input, input}, state) do
    try do
      {rc, newbindings} = Code.eval_string(input, state.bindings)
      newstate = %{state | bindings: newbindings}
      cast_port(state, :rc, [inspect rc])
      cast_port(state, :prompt, [])
      {:noreply, newstate}
    catch
      kind, reason ->
        cast_port(state, :error, ["** (#{kind}) #{inspect reason}"]) #  "(#{err}) #{msg}")
        cast_port(state, :prompt, [])
        {:noreply, state}
    end
  end

end

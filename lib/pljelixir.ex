defmodule Pljelixir do
  use Application.Behaviour

  # See http://elixir-lang.org/docs/stable/Application.Behaviour.html
  # for more information on OTP Applications
  def start(_type, _args) do
    # Initialize networking
    :os.cmd('/sbin/ip link set eth0 up')
    :os.cmd('/sbin/ip addr add 192.168.1.40/24 dev eth0')
    :os.cmd('/sbin/ip route add default via 192.168.1.1')

    Pljelixir.Supervisor.start_link
  end
end

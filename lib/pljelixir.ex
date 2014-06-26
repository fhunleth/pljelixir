defmodule Pljelixir do
  use Application

  # See http://elixir-lang.org/docs/stable/Application.Behaviour.html
  # for more information on OTP Applications
  def start(_type, _args) do
    # Initialize networking
    System.cmd("/sbin/ip link set eth0 up")
    System.cmd("/sbin/ip addr add 192.168.1.40/24 dev eth0")
    System.cmd("/sbin/ip route add default via 192.168.1.1")

    Pljelixir.Supervisor.start_link
  end
end

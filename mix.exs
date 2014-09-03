defmodule Mix.Tasks.Compile.Pljelixir do
  @shortdoc "Compiles C code"

  def run(_) do
    0=Mix.Shell.IO.cmd("make compile_port")
  end
end

defmodule Pljelixir.Mixfile do
  use Mix.Project

  def project do
    [app: :pljelixir,
     version: "0.0.1",
     elixir: "> 0.15.0",
     compilers: [:Pljelixir, :elixir, :app],
     deps: deps]
  end

  # Configuration for the OTP application
  #
  # Type `mix help compile.app` for more information
  def application do
    [ applications: [],
      mod: { Pljelixir, [] } ]
  end

  # List all dependencies in the format:
  #
  # { :foobar, git: "https://github.com/elixir-lang/foobar.git", tag: "0.1" }
  #
  # Type `mix help deps` for more examples and options
  defp deps do
    [
      {:lager, github: "basho/lager", tag: "2.0.3", override: true},
      {:exlager, github: "khia/exlager"},
      {:l2elog, github: "fhunleth/l2elog"},
      {:elixir_ale, github: "fhunleth/elixir_ale", tag: "master"}
    ]
  end
end

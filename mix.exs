defmodule Mix.Tasks.Compile.Console do
  @shortdoc "Compiles C code"

  def run(_) do
    Mix.shell.info System.cmd("make compile_port")
  end
end

defmodule Console.Mixfile do
  use Mix.Project

  def project do
    [app: :console,
     version: "0.0.1",
     elixir: "~> 0.13.0",
     compilers: [:Console, :elixir, :app],
     deps: deps]
  end

  # Configuration for the OTP application
  #
  # Type `mix help compile.app` for more information
  def application do
    [ applications: [],
      mod: { Console, [] } ]
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
      {:l2elog, github: "fhunleth/l2elog"}
	]
  end
end

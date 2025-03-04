defmodule Pop.MixProject do
  use Mix.Project

  def project do
    [
      app: :pop,
      version: "0.5.1",
      elixir: "~> 1.12",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      escript: escript(),
      erlc_paths: ["src"],
      compilers: [:leex, :yecc] ++ Mix.compilers()
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    []
  end

  defp escript do
    [
      main_module: PopCli,
      name: "pop",
      app: nil,
      path: "pop"
    ]
  end
end

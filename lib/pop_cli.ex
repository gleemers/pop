defmodule PopCli do
  @moduledoc """
  CLI interface for the Pop language.
  """

  @doc """
  Main entry point for the CLI.
  """
  def main(args) do
    args
    |> parse_args()
    |> process()
  end

  @doc """
  Parse command line arguments.
  """
  def parse_args(args) do
    {opts, cmd_args, _} = OptionParser.parse(args,
      switches: [
        help: :boolean,
        version: :boolean
      ],
      aliases: [
        h: :help,
        v: :version
      ]
    )

    {opts, cmd_args}
  end

  @doc """
  Process the parsed arguments.
  """
  def process({opts, []}) do
    cond do
      opts[:help] -> print_help()
      opts[:version] -> print_version()
      true -> print_help()
    end
  end

  def process({opts, ["compile", filename]}) do
    if opts[:help] do
      print_compile_help()
    else
      compile_file(filename)
    end
  end

  def process({opts, ["run", filename]}) do
    if opts[:help] do
      print_run_help()
    else
      run_file(filename)
    end
  end

  def process({_opts, [filename]}) do
    compile_and_run_file(filename)
  end

  def process({_opts, _}) do
    IO.puts("Invalid command")
    print_help()
  end

  @doc """
  Print help information.
  """
  def print_help do
    IO.puts("""
    Pop Language Compiler

    Usage:
      pop [options] <command> [arguments]

    Commands:
      compile <filename>  Compile a Pop source file
      run <filename>      Run a compiled Pop file
      <filename>          Compile and run a Pop file

    Options:
      -h, --help          Show this help message
      -v, --version       Show version information

    Examples:
      pop examples/hello.pop
      pop compile examples/factorial.pop
      pop run examples/factorial.pop
      pop --help
    """)
  end

  @doc """
  Print compile command help.
  """
  def print_compile_help do
    IO.puts("""
    Pop Language Compiler - Compile Command

    Usage:
      pop compile <filename>

    Description:
      Compiles a Pop source file to BEAM bytecode.

    Examples:
      pop compile examples/hello.pop
    """)
  end

  @doc """
  Print run command help.
  """
  def print_run_help do
    IO.puts("""
    Pop Language Compiler - Run Command

    Usage:
      pop run <filename>

    Description:
      Runs a compiled Pop file.

    Examples:
      pop run examples/hello.pop
    """)
  end

  @doc """
  Print version information.
  """
  def print_version do
    IO.puts("Pop Language Compiler v0.1.0")
  end

  @doc """
  Compile a Pop source file.
  """
  def compile_file(filename) do
    case :pop_compiler.compile_file(filename) do
      {:ok, module} ->
        IO.puts("Compilation successful: #{filename}")
        {:ok, module}
      {:error, reason} ->
        IO.puts("Compilation failed: #{inspect(reason)}")
        {:error, reason}
    end
  end

  @doc """
  Run a compiled Pop file.
  """
  def run_file(filename) do
    case compile_file(filename) do
      {:ok, module} ->
        try do
          apply(module, :main, [])
          :ok
        rescue
          e ->
            IO.puts("Runtime error: #{inspect(e)}")
            {:error, e}
        end
      error -> error
    end
  end

  @doc """
  Compile and run a Pop file.
  """
  def compile_and_run_file(filename) do
    run_file(filename)
  end
end

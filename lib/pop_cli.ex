defmodule PopCli do
  @moduledoc """
  CLI interface for the Pop language.
  """

  # ANSI color codes for general CLI output
  @green IO.ANSI.green()
  @cyan IO.ANSI.cyan()
  @yellow IO.ANSI.yellow()
  @blue IO.ANSI.blue()
  @magenta IO.ANSI.magenta()
  @reset IO.ANSI.reset()
  @bold IO.ANSI.bright()
  @underline IO.ANSI.underline()

  @doc """
  Main entry point for the CLI.
  """
  def main(args) do
    IO.puts("\n#{@cyan}#{@bold}Pop Language#{@reset} #{@magenta}v0.5.0#{@reset}\n")

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
      IO.puts("#{@cyan}#{@bold}Compiling:#{@reset} #{@blue}#{filename}#{@reset}\n")
      compile_file(filename)
    end
  end

  def process({opts, ["run", filename]}) do
    if opts[:help] do
      print_run_help()
    else
      IO.puts("#{@cyan}#{@bold}Running:#{@reset} #{@blue}#{filename}#{@reset}\n")
      run_file(filename)
    end
  end

  def process({_opts, [filename]}) do
    IO.puts("#{@cyan}#{@bold}Compiling and running:#{@reset} #{@blue}#{filename}#{@reset}\n")
    compile_and_run_file(filename)
  end

  def process({_opts, _}) do
    IO.puts(PopErrorFormatter.format_error("Invalid command"))
    print_help()
  end

  @doc """
  Print help information.
  """
  def print_help do
    IO.puts("""
    #{@cyan}#{@bold}#{@underline}Pop Language Compiler#{@reset}

    #{@bold}Usage:#{@reset}
      pop [options] <command> [arguments]

    #{@bold}#{@yellow}Commands:#{@reset}
      #{@green}compile#{@reset} <filename>  Compile a Pop source file
      #{@green}run#{@reset} <filename>      Run a compiled Pop file
      <filename>          Compile and run a Pop file

    #{@bold}#{@yellow}Options:#{@reset}
      #{@blue}-h, --help#{@reset}          Show this help message
      #{@blue}-v, --version#{@reset}       Show version information

    #{@bold}#{@yellow}Examples:#{@reset}
      #{@magenta}pop examples/hello.pop#{@reset}
      #{@magenta}pop compile examples/factorial.pop#{@reset}
      #{@magenta}pop run examples/factorial.pop#{@reset}
      #{@magenta}pop --help#{@reset}
    """)
  end

  @doc """
  Print compile command help.
  """
  def print_compile_help do
    IO.puts("""
    #{@cyan}#{@bold}#{@underline}Pop Language Compiler - Compile Command#{@reset}

    #{@bold}Usage:#{@reset}
      pop compile <filename>

    #{@bold}Description:#{@reset}
      Compiles a Pop source file to BEAM bytecode.

    #{@bold}#{@yellow}Examples:#{@reset}
      #{@magenta}pop compile examples/hello.pop#{@reset}
    """)
  end

  @doc """
  Print run command help.
  """
  def print_run_help do
    IO.puts("""
    #{@cyan}#{@bold}#{@underline}Pop Language Compiler - Run Command#{@reset}

    #{@bold}Usage:#{@reset}
      pop run <filename>

    #{@bold}Description:#{@reset}
      Runs a compiled Pop file.

    #{@bold}#{@yellow}Examples:#{@reset}
      #{@magenta}pop run examples/hello.pop#{@reset}
    """)
  end

  @doc """
  Print version information.
  """
  def print_version do
    IO.puts("""
    #{@cyan}#{@bold}#{@underline}Pop Language Compiler#{@reset} #{@green}v0.5.0#{@reset}

    #{@bold}Created by:#{@reset} Thoq
    #{@bold}License:#{@reset} MIT

    A simple functional programming language that compiles to BEAM bytecode.
    """)
  end

  @doc """
  Compile a Pop source file.
  """
  def compile_file(filename) do
    case File.exists?(filename) do
      true ->
        try do
          result = :pop_compiler.compile_file(filename)
          case result do
            {:ok, module} ->
              IO.puts(PopErrorFormatter.format_success("Compilation successful: #{filename}"))
              {:ok, module}
            {:ok, module, warnings} ->
              # Compilation successful but with warnings
              IO.puts(PopErrorFormatter.format_success("Compilation successful: #{filename}"))
              if warnings && !Enum.empty?(warnings) do
                IO.puts(PopErrorReporter.format_warnings(warnings, filename))
              end
              {:ok, module}
            error = {:error, {_line, :pop_lexer, {:illegal, _token}}, _} ->
              IO.puts(PopErrorReporter.enhance_error(error, filename))
              error
            error = {:error, {_line, :pop_parser, _}} ->
              IO.puts(PopErrorReporter.enhance_error(error, filename))
              error
            error = {:error, errors, warnings} when is_list(errors) ->
              IO.puts(PopErrorReporter.enhance_error(error, filename))
              error
            error = {:error, reason} ->
              IO.puts(PopErrorFormatter.format_error("Compilation failed: #{inspect(reason)}"))
              error
          end
        rescue
          e ->
            IO.puts(PopErrorFormatter.format_error("Compilation error: #{inspect(e)}"))
            {:error, e}
        end
      false ->
        IO.puts(PopErrorFormatter.format_error("File not found: #{filename}"))
        {:error, :file_not_found}
    end
  end

  @doc """
  Run a compiled Pop file.
  """
  def run_file(filename) do
    result = compile_file(filename)
    case result do
      {:ok, module} ->
        try do
          IO.puts("\n#{@green}#{@bold}Program Output:#{@reset}\n#{@yellow}#{@bold}----------------#{@reset}")
          result = apply(module, :main, [])
          IO.puts("#{@yellow}#{@bold}----------------#{@reset}")
          IO.puts("\n#{@green}#{@bold}Program completed successfully.#{@reset}")
          result
        rescue
          e ->
            IO.puts(PopErrorFormatter.format_runtime_error(e))
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

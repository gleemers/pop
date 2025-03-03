defmodule PopTest do
  use ExUnit.Case
  doctest Pop

  test "version returns a string" do
    assert is_binary(Pop.version())
  end

  test "PopCli.parse_args parses help flag" do
    {opts, args} = PopCli.parse_args(["--help"])
    assert opts[:help] == true
    assert args == []
  end

  test "PopCli.parse_args parses version flag" do
    {opts, args} = PopCli.parse_args(["--version"])
    assert opts[:version] == true
    assert args == []
  end

  test "PopCli.parse_args parses compile command" do
    {opts, args} = PopCli.parse_args(["compile", "test.pop"])
    assert args == ["compile", "test.pop"]
  end

  test "PopCli.parse_args parses run command" do
    {opts, args} = PopCli.parse_args(["run", "test.pop"])
    assert args == ["run", "test.pop"]
  end
end

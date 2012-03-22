Feature: command line utility

  All the functionality should be available through a fancy command
  line interface. In particular it should be possible to:
    - preprocess fortran files
    - generate a program from .cg files
    - check the constraints of the application

  Scenario: preprocessing fortran files
    Given the cwd is "examples/testdata"
    And user defined macros in "macros/"
    When I run "ppm pp fortran.f -o output/preprocessed.f"
    Then the result file is equal to "fortran_gold.f"

  Scenario: big fortran
    Given the cwd is "examples/testdata"
    And user defined macros in "macros/"
    When I run "ppm pp big_fortran.f -o output/preprocessed.f"
    Then the result file is equal to "big_fortran_gold.f"

   Scenario: creating a project skeleton
     Given the cwd is "examples"
     When I run "ppm newproject project_1 -d"
     Then the project skeleton is created in "examples/project_1"

  #  Scenario: generating fortran source from client generator files
  #    Given the cwd is "examples/testproject"
  #    When I run "ppm gen"
  #    Then I get the generated fortran source files and all necessary auxillary files in "examples/testproject/gen"
  
  # Scenario: compiling client generator files
  #   Given the cwd is "examples/testproject"
  #   When I run "ppm build"
  #   Then I get an executable simulation

  # Scenario: running simulations
  #   Given the cwd is "examples/testproject"
  #   When I run "ppm run"
  #   Then the simulation is executed

  # Scenario: cleaning the project directory
  #   Given the cwd is "examples/testproject"
  #   When I run "ppm clean"
  #   Then the build directory is deleted

  # Scenario: setting configuration options
  #   Given the cwd is "examples/testproject"
  #   When I run "ppm config compiler=gfortran"
  #   Then the config option "compiler" is set to "gfortran"

  # Scenario: reading configuration options
  #   Given the cwd is "examples/testproject"
  #   And the config option "compiler" is set to "gfortran"
  #   When I run "ppm config compiler"
  #   Then the command outputs "gfortran"

  # Scenario: prompt for missing options interactively
  #   Given the cwd is "examples/testproject"
  #   When I run "ppm run"
  #   Then I get a prompt to enter the missing configuration options

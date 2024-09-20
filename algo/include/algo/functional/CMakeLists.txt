target_sources(${PROJECT_NAME}
  INTERFACE
  FILE_SET ${PROJECT_NAME}_HEADERS
  TYPE HEADERS
    FILES
        applicative.hpp
        foldable.hpp
        function.hpp
        functor.hpp
        monad.hpp
        monoid.hpp
    )

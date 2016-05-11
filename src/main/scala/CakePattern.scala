
// based on yt: https://www.youtube.com/watch?v=DysTHmpDgvk
// Peter Potts: Cake Pattern in Practice

// TODO
// Cake Pattern: The Bakery from the Black Lagoon
// https://www.youtube.com/watch?v=yLbdw06tKPQ

object CakePattern {

  object CakePatterninPractice {

    /*
     * interface
     * implementation
     * wiring
    */

    // Interface
    trait VehicleComponent {
      val vehicle: Vehicle
      trait Vehicle
    }

    // Implementation
    object CarComponent { // keep companion object with dependencies
      type Dependencies = FuelComponent with RoadComponent
    }

    trait CarComponent extends VehicleComponent {
      self: CarComponent.Dependencies =>

      class Car extends Vehicle {
        fuel.#.#
        road.#.#
      }
    }

    // Wiring
    object CarWiring {
      type Dependencies = CarComponent.Dependencies
    }

    trait CarWiring extends CarComponent {
      self: CarWiring.Dependencies =>

      lazy val car = new Car // deps could not be initialized yet
    }

    // Wiring - multiple component wiring
    object TransportWiring {
      type Dependencies = CarWiring.Dependencies
        with DieselComponent.Dependencies
        with HighwayComponent.Dependencies
    }

    trait TransportWiring
       extends CarWiring
       with DieselComponent
       with HighwayComponent
       self: ModuleWiring.Dependencies =>

       lazy val fuel = new Diesel
       lazy val road = new Highway

    // Rules:
    object One_Access_Point_Per_Component {

      trait VehicleComponent {
        val capacity: Capacity // access point for VehicleComponent
        val shape: Shape // access point for VehicleComponent
      }

      object UseInstead {
        trait VehicleComponent {

          trait Vehicle { // access point for VehicleComponent
            val capacity: Capacity
            val shape: Shape
          }
        }
      }

    }
  }

}

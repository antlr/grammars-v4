// Abstract classes can't be instantiated.
abstract class Item {
  void use();
}

// Classes can implement other classes.
class Chest<T> implements Item {
  final List<T> contents;

  // Constructors can assign arguments to instance variables using `this`.
  Chest(this.contents);

  @override
  void use() => print('$this has ${contents.length} items.');
}

class Sword implements Item {
  int get damage => 5;

  @override
  void use() => print('$this dealt $damage damage.');
}

// Classes can extend other classes.
class DiamondSword extends Sword {
  @override
  final int damage = 50;
}

void main() {
  // The 'new' keyword is optional.
  var chest = Chest<Item>([
    DiamondSword(),
    Sword(),
  ]);

  chest.use();

  for (final item in chest.contents) {
    item.use();
  }
}
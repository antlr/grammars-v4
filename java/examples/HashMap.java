package java.util;

import java.io.IOException;
import java.io.InvalidObjectException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import sun.misc.SharedSecrets;

public class HashMap extends AbstractMap implements Map, Cloneable, Serializable {
   private static final long serialVersionUID = 362498820763181265L;
   static final int DEFAULT_INITIAL_CAPACITY = 16;
   static final int MAXIMUM_CAPACITY = 1073741824;
   static final float DEFAULT_LOAD_FACTOR = 0.75F;
   static final int TREEIFY_THRESHOLD = 8;
   static final int UNTREEIFY_THRESHOLD = 6;
   static final int MIN_TREEIFY_CAPACITY = 64;
   transient HashMap.Node[] table;
   transient Set entrySet;
   transient int size;
   transient int modCount;
   int threshold;
   final float loadFactor;

   static final int hash(Object var0) {
      int var1;
      return var0 == null ? 0 : (var1 = var0.hashCode()) ^ var1 >>> 16;
   }

   static Class comparableClassFor(Object var0) {
      if (var0 instanceof Comparable) {
         Class var1;
         if ((var1 = var0.getClass()) == String.class) {
            return var1;
         }

         Type[] var2;
         if ((var2 = var1.getGenericInterfaces()) != null) {
            for(int var6 = 0; var6 < var2.length; ++var6) {
               Type[] var3;
               Type var4;
               ParameterizedType var5;
               if ((var4 = var2[var6]) instanceof ParameterizedType && (var5 = (ParameterizedType)var4).getRawType() == Comparable.class && (var3 = var5.getActualTypeArguments()) != null && var3.length == 1 && var3[0] == var1) {
                  return var1;
               }
            }
         }
      }

      return null;
   }

   static int compareComparables(Class var0, Object var1, Object var2) {
      return var2 != null && var2.getClass() == var0 ? ((Comparable)var1).compareTo(var2) : 0;
   }

   static final int tableSizeFor(int var0) {
      int var1 = var0 - 1;
      var1 |= var1 >>> 1;
      var1 |= var1 >>> 2;
      var1 |= var1 >>> 4;
      var1 |= var1 >>> 8;
      var1 |= var1 >>> 16;
      return var1 < 0 ? 1 : (var1 >= 1073741824 ? 1073741824 : var1 + 1);
   }

   public HashMap(int var1, float var2) {
      super();
      if (var1 < 0) {
         throw new IllegalArgumentException("Illegal initial capacity: " + var1);
      } else {
         if (var1 > 1073741824) {
            var1 = 1073741824;
         }

         if (var2 > 0.0F && !Float.isNaN(var2)) {
            this.loadFactor = var2;
            this.threshold = tableSizeFor(var1);
         } else {
            throw new IllegalArgumentException("Illegal load factor: " + var2);
         }
      }
   }

   public HashMap(int var1) {
      this(var1, 0.75F);
   }

   public HashMap() {
      super();
      this.loadFactor = 0.75F;
   }

   public HashMap(Map var1) {
      super();
      this.loadFactor = 0.75F;
      this.putMapEntries(var1, false);
   }

   final void putMapEntries(Map var1, boolean var2) {
      int var3 = var1.size();
      if (var3 > 0) {
         if (this.table == null) {
            float var4 = (float)var3 / this.loadFactor + 1.0F;
            int var5 = var4 < 1.07374182E9F ? (int)var4 : 1073741824;
            if (var5 > this.threshold) {
               this.threshold = tableSizeFor(var5);
            }
         } else if (var3 > this.threshold) {
            this.resize();
         }

         Iterator var8 = var1.entrySet().iterator();

         while(var8.hasNext()) {
            Map.Entry var9 = (Map.Entry)var8.next();
            Object var6 = var9.getKey();
            Object var7 = var9.getValue();
            this.putVal(hash(var6), var6, var7, false, var2);
         }
      }

   }

   public int size() {
      return this.size;
   }

   public boolean isEmpty() {
      return this.size == 0;
   }

   public Object get(Object var1) {
      HashMap.Node var2;
      return (var2 = this.getNode(hash(var1), var1)) == null ? null : var2.value;
   }

   final HashMap.Node getNode(int var1, Object var2) {
      HashMap.Node[] var3 = this.table;
      HashMap.Node var4;
      int var6;
      if (this.table != null && (var6 = var3.length) > 0 && (var4 = var3[var6 - 1 & var1]) != null) {
         Object var7;
         if (var4.hash == var1) {
            var7 = var4.key;
            if (var4.key == var2 || var2 != null && var2.equals(var7)) {
               return var4;
            }
         }

         HashMap.Node var5 = var4.next;
         if (var4.next != null) {
            if (var4 instanceof HashMap.TreeNode) {
               return ((HashMap.TreeNode)var4).getTreeNode(var1, var2);
            }

            do {
               if (var5.hash == var1) {
                  var7 = var5.key;
                  if (var5.key == var2 || var2 != null && var2.equals(var7)) {
                     return var5;
                  }
               }
            } while((var5 = var5.next) != null);
         }
      }

      return null;
   }

   public boolean containsKey(Object var1) {
      return this.getNode(hash(var1), var1) != null;
   }

   public Object put(Object var1, Object var2) {
      return this.putVal(hash(var1), var1, var2, false, true);
   }

   final Object putVal(int var1, Object var2, Object var3, boolean var4, boolean var5) {
      HashMap.Node[] var6 = this.table;
      int var8;
      if (this.table == null || (var8 = var6.length) == 0) {
         var8 = (var6 = this.resize()).length;
      }

      Object var7;
      int var9;
      if ((var7 = var6[var9 = var8 - 1 & var1]) == null) {
         var6[var9] = this.newNode(var1, var2, var3, (HashMap.Node)null);
      } else {
         Object var10;
         label79: {
            Object var11;
            if (((HashMap.Node)var7).hash == var1) {
               var11 = ((HashMap.Node)var7).key;
               if (((HashMap.Node)var7).key == var2 || var2 != null && var2.equals(var11)) {
                  var10 = var7;
                  break label79;
               }
            }

            if (var7 instanceof HashMap.TreeNode) {
               var10 = ((HashMap.TreeNode)var7).putTreeVal(this, var6, var1, var2, var3);
            } else {
               int var12 = 0;

               while(true) {
                  var10 = ((HashMap.Node)var7).next;
                  if (((HashMap.Node)var7).next == null) {
                     ((HashMap.Node)var7).next = this.newNode(var1, var2, var3, (HashMap.Node)null);
                     if (var12 >= 7) {
                        this.treeifyBin(var6, var1);
                     }
                     break;
                  }

                  if (((HashMap.Node)var10).hash == var1) {
                     var11 = ((HashMap.Node)var10).key;
                     if (((HashMap.Node)var10).key == var2 || var2 != null && var2.equals(var11)) {
                        break;
                     }
                  }

                  var7 = var10;
                  ++var12;
               }
            }
         }

         if (var10 != null) {
            Object var13 = ((HashMap.Node)var10).value;
            if (!var4 || var13 == null) {
               ((HashMap.Node)var10).value = var3;
            }

            this.afterNodeAccess((HashMap.Node)var10);
            return var13;
         }
      }

      ++this.modCount;
      if (++this.size > this.threshold) {
         this.resize();
      }

      this.afterNodeInsertion(var5);
      return null;
   }

   final HashMap.Node[] resize() {
      HashMap.Node[] var1 = this.table;
      int var2 = var1 == null ? 0 : var1.length;
      int var3 = this.threshold;
      int var5 = 0;
      int var4;
      if (var2 > 0) {
         if (var2 >= 1073741824) {
            this.threshold = Integer.MAX_VALUE;
            return var1;
         }

         if ((var4 = var2 << 1) < 1073741824 && var2 >= 16) {
            var5 = var3 << 1;
         }
      } else if (var3 > 0) {
         var4 = var3;
      } else {
         var4 = 16;
         var5 = 12;
      }

      if (var5 == 0) {
         float var6 = (float)var4 * this.loadFactor;
         var5 = var4 < 1073741824 && var6 < 1.07374182E9F ? (int)var6 : Integer.MAX_VALUE;
      }

      this.threshold = var5;
      HashMap.Node[] var14 = (HashMap.Node[])(new HashMap.Node[var4]);
      this.table = var14;
      if (var1 != null) {
         for(int var7 = 0; var7 < var2; ++var7) {
            HashMap.Node var8;
            if ((var8 = var1[var7]) != null) {
               var1[var7] = null;
               if (var8.next == null) {
                  var14[var8.hash & var4 - 1] = var8;
               } else if (var8 instanceof HashMap.TreeNode) {
                  ((HashMap.TreeNode)var8).split(this, var14, var7, var2);
               } else {
                  HashMap.Node var9 = null;
                  HashMap.Node var10 = null;
                  HashMap.Node var11 = null;
                  HashMap.Node var12 = null;

                  HashMap.Node var13;
                  do {
                     var13 = var8.next;
                     if ((var8.hash & var2) == 0) {
                        if (var10 == null) {
                           var9 = var8;
                        } else {
                           var10.next = var8;
                        }

                        var10 = var8;
                     } else {
                        if (var12 == null) {
                           var11 = var8;
                        } else {
                           var12.next = var8;
                        }

                        var12 = var8;
                     }

                     var8 = var13;
                  } while(var13 != null);

                  if (var10 != null) {
                     var10.next = null;
                     var14[var7] = var9;
                  }

                  if (var12 != null) {
                     var12.next = null;
                     var14[var7 + var2] = var11;
                  }
               }
            }
         }
      }

      return var14;
   }

   final void treeifyBin(HashMap.Node[] var1, int var2) {
      int var3;
      if (var1 != null && (var3 = var1.length) >= 64) {
         int var4;
         HashMap.Node var5;
         if ((var5 = var1[var4 = var3 - 1 & var2]) != null) {
            HashMap.TreeNode var6 = null;
            HashMap.TreeNode var7 = null;

            do {
               HashMap.TreeNode var8 = this.replacementTreeNode(var5, (HashMap.Node)null);
               if (var7 == null) {
                  var6 = var8;
               } else {
                  var8.prev = var7;
                  var7.next = var8;
               }

               var7 = var8;
            } while((var5 = var5.next) != null);

            if ((var1[var4] = var6) != null) {
               var6.treeify(var1);
            }
         }
      } else {
         this.resize();
      }

   }

   public void putAll(Map var1) {
      this.putMapEntries(var1, true);
   }

   public Object remove(Object var1) {
      HashMap.Node var2;
      return (var2 = this.removeNode(hash(var1), var1, (Object)null, false, true)) == null ? null : var2.value;
   }

   final HashMap.Node removeNode(int var1, Object var2, Object var3, boolean var4, boolean var5) {
      HashMap.Node[] var6;
      HashMap.Node var7;
      int var9;
      Object var10;
      label89: {
         var6 = this.table;
         int var8;
         if (this.table != null && (var8 = var6.length) > 0 && (var7 = var6[var9 = var8 - 1 & var1]) != null) {
            label90: {
               var10 = null;
               Object var12;
               if (var7.hash == var1) {
                  var12 = var7.key;
                  if (var7.key == var2 || var2 != null && var2.equals(var12)) {
                     var10 = var7;
                     break label90;
                  }
               }

               HashMap.Node var11 = var7.next;
               if (var7.next != null) {
                  if (var7 instanceof HashMap.TreeNode) {
                     var10 = ((HashMap.TreeNode)var7).getTreeNode(var1, var2);
                  } else {
                     do {
                        if (var11.hash == var1) {
                           var12 = var11.key;
                           if (var11.key == var2 || var2 != null && var2.equals(var12)) {
                              var10 = var11;
                              break;
                           }
                        }

                        var7 = var11;
                     } while((var11 = var11.next) != null);
                  }
               }
            }

            if (var10 != null) {
               if (!var4) {
                  break label89;
               }

               Object var13 = ((HashMap.Node)var10).value;
               if (((HashMap.Node)var10).value == var3 || var3 != null && var3.equals(var13)) {
                  break label89;
               }
            }
         }

         return null;
      }

      if (var10 instanceof HashMap.TreeNode) {
         ((HashMap.TreeNode)var10).removeTreeNode(this, var6, var5);
      } else if (var10 == var7) {
         var6[var9] = ((HashMap.Node)var10).next;
      } else {
         var7.next = ((HashMap.Node)var10).next;
      }

      ++this.modCount;
      --this.size;
      this.afterNodeRemoval((HashMap.Node)var10);
      return (HashMap.Node)var10;
   }

   public void clear() {
      ++this.modCount;
      HashMap.Node[] var1 = this.table;
      if (this.table != null && this.size > 0) {
         this.size = 0;

         for(int var2 = 0; var2 < var1.length; ++var2) {
            var1[var2] = null;
         }
      }

   }

   public boolean containsValue(Object var1) {
      HashMap.Node[] var2 = this.table;
      if (this.table != null && this.size > 0) {
         for(int var4 = 0; var4 < var2.length; ++var4) {
            for(HashMap.Node var5 = var2[var4]; var5 != null; var5 = var5.next) {
               Object var3 = var5.value;
               if (var5.value == var1 || var1 != null && var1.equals(var3)) {
                  return true;
               }
            }
         }
      }

      return false;
   }

   public Set keySet() {
      Object var1 = this.keySet;
      if (var1 == null) {
         var1 = new HashMap.KeySet();
         this.keySet = (Set)var1;
      }

      return (Set)var1;
   }

   public Collection values() {
      Object var1 = this.values;
      if (var1 == null) {
         var1 = new HashMap.Values();
         this.values = (Collection)var1;
      }

      return (Collection)var1;
   }

   public Set entrySet() {
      Set var1 = this.entrySet;
      return this.entrySet == null ? (this.entrySet = new HashMap.EntrySet()) : var1;
   }

   public Object getOrDefault(Object var1, Object var2) {
      HashMap.Node var3;
      return (var3 = this.getNode(hash(var1), var1)) == null ? var2 : var3.value;
   }

   public Object putIfAbsent(Object var1, Object var2) {
      return this.putVal(hash(var1), var1, var2, true, true);
   }

   public boolean remove(Object var1, Object var2) {
      return this.removeNode(hash(var1), var1, var2, true, true) != null;
   }

   public boolean replace(Object var1, Object var2, Object var3) {
      HashMap.Node var4;
      if ((var4 = this.getNode(hash(var1), var1)) != null) {
         Object var5 = var4.value;
         if (var4.value == var2 || var5 != null && var5.equals(var2)) {
            var4.value = var3;
            this.afterNodeAccess(var4);
            return true;
         }
      }

      return false;
   }

   public Object replace(Object var1, Object var2) {
      HashMap.Node var3;
      if ((var3 = this.getNode(hash(var1), var1)) != null) {
         Object var4 = var3.value;
         var3.value = var2;
         this.afterNodeAccess(var3);
         return var4;
      } else {
         return null;
      }
   }

   public Object computeIfAbsent(Object var1, Function var2) {
      if (var2 == null) {
         throw new NullPointerException();
      } else {
         int var3;
         HashMap.Node[] var4;
         int var6;
         int var8;
         HashMap.TreeNode var9;
         Object var10;
         label63: {
            var3 = hash(var1);
            var8 = 0;
            var9 = null;
            var10 = null;
            if (this.size <= this.threshold) {
               var4 = this.table;
               if (this.table != null && (var6 = var4.length) != 0) {
                  break label63;
               }
            }

            var6 = (var4 = this.resize()).length;
         }

         HashMap.Node var5;
         int var7;
         Object var13;
         if ((var5 = var4[var7 = var6 - 1 & var3]) != null) {
            if (var5 instanceof HashMap.TreeNode) {
               var10 = (var9 = (HashMap.TreeNode)var5).getTreeNode(var3, var1);
            } else {
               HashMap.Node var11 = var5;

               do {
                  if (var11.hash == var3) {
                     Object var12 = var11.key;
                     if (var11.key == var1 || var1 != null && var1.equals(var12)) {
                        var10 = var11;
                        break;
                     }
                  }

                  ++var8;
               } while((var11 = var11.next) != null);
            }

            if (var10 != null) {
               var13 = ((HashMap.Node)var10).value;
               if (((HashMap.Node)var10).value != null) {
                  this.afterNodeAccess((HashMap.Node)var10);
                  return var13;
               }
            }
         }

         var13 = var2.apply(var1);
         if (var13 == null) {
            return null;
         } else if (var10 != null) {
            ((HashMap.Node)var10).value = var13;
            this.afterNodeAccess((HashMap.Node)var10);
            return var13;
         } else {
            if (var9 != null) {
               var9.putTreeVal(this, var4, var3, var1, var13);
            } else {
               var4[var7] = this.newNode(var3, var1, var13, var5);
               if (var8 >= 7) {
                  this.treeifyBin(var4, var3);
               }
            }

            ++this.modCount;
            ++this.size;
            this.afterNodeInsertion(true);
            return var13;
         }
      }
   }

   public Object computeIfPresent(Object var1, BiFunction var2) {
      if (var2 == null) {
         throw new NullPointerException();
      } else {
         int var5 = hash(var1);
         HashMap.Node var3;
         if ((var3 = this.getNode(var5, var1)) != null) {
            Object var4 = var3.value;
            if (var3.value != null) {
               Object var6 = var2.apply(var1, var4);
               if (var6 != null) {
                  var3.value = var6;
                  this.afterNodeAccess(var3);
                  return var6;
               }

               this.removeNode(var5, var1, (Object)null, false, true);
            }
         }

         return null;
      }
   }

   public Object compute(Object var1, BiFunction var2) {
      if (var2 == null) {
         throw new NullPointerException();
      } else {
         int var3;
         HashMap.Node[] var4;
         int var6;
         int var8;
         HashMap.TreeNode var9;
         Object var10;
         label63: {
            var3 = hash(var1);
            var8 = 0;
            var9 = null;
            var10 = null;
            if (this.size <= this.threshold) {
               var4 = this.table;
               if (this.table != null && (var6 = var4.length) != 0) {
                  break label63;
               }
            }

            var6 = (var4 = this.resize()).length;
         }

         HashMap.Node var5;
         int var7;
         Object var12;
         if ((var5 = var4[var7 = var6 - 1 & var3]) != null) {
            if (var5 instanceof HashMap.TreeNode) {
               var10 = (var9 = (HashMap.TreeNode)var5).getTreeNode(var3, var1);
            } else {
               HashMap.Node var11 = var5;

               do {
                  if (var11.hash == var3) {
                     var12 = var11.key;
                     if (var11.key == var1 || var1 != null && var1.equals(var12)) {
                        var10 = var11;
                        break;
                     }
                  }

                  ++var8;
               } while((var11 = var11.next) != null);
            }
         }

         Object var13 = var10 == null ? null : ((HashMap.Node)var10).value;
         var12 = var2.apply(var1, var13);
         if (var10 != null) {
            if (var12 != null) {
               ((HashMap.Node)var10).value = var12;
               this.afterNodeAccess((HashMap.Node)var10);
            } else {
               this.removeNode(var3, var1, (Object)null, false, true);
            }
         } else if (var12 != null) {
            if (var9 != null) {
               var9.putTreeVal(this, var4, var3, var1, var12);
            } else {
               var4[var7] = this.newNode(var3, var1, var12, var5);
               if (var8 >= 7) {
                  this.treeifyBin(var4, var3);
               }
            }

            ++this.modCount;
            ++this.size;
            this.afterNodeInsertion(true);
         }

         return var12;
      }
   }

   public Object merge(Object var1, Object var2, BiFunction var3) {
      if (var2 == null) {
         throw new NullPointerException();
      } else if (var3 == null) {
         throw new NullPointerException();
      } else {
         int var4;
         HashMap.Node[] var5;
         int var7;
         int var9;
         HashMap.TreeNode var10;
         Object var11;
         label68: {
            var4 = hash(var1);
            var9 = 0;
            var10 = null;
            var11 = null;
            if (this.size <= this.threshold) {
               var5 = this.table;
               if (this.table != null && (var7 = var5.length) != 0) {
                  break label68;
               }
            }

            var7 = (var5 = this.resize()).length;
         }

         HashMap.Node var6;
         int var8;
         if ((var6 = var5[var8 = var7 - 1 & var4]) != null) {
            if (var6 instanceof HashMap.TreeNode) {
               var11 = (var10 = (HashMap.TreeNode)var6).getTreeNode(var4, var1);
            } else {
               HashMap.Node var12 = var6;

               do {
                  if (var12.hash == var4) {
                     Object var13 = var12.key;
                     if (var12.key == var1 || var1 != null && var1.equals(var13)) {
                        var11 = var12;
                        break;
                     }
                  }

                  ++var9;
               } while((var12 = var12.next) != null);
            }
         }

         if (var11 != null) {
            Object var14;
            if (((HashMap.Node)var11).value != null) {
               var14 = var3.apply(((HashMap.Node)var11).value, var2);
            } else {
               var14 = var2;
            }

            if (var14 != null) {
               ((HashMap.Node)var11).value = var14;
               this.afterNodeAccess((HashMap.Node)var11);
            } else {
               this.removeNode(var4, var1, (Object)null, false, true);
            }

            return var14;
         } else {
            if (var2 != null) {
               if (var10 != null) {
                  var10.putTreeVal(this, var5, var4, var1, var2);
               } else {
                  var5[var8] = this.newNode(var4, var1, var2, var6);
                  if (var9 >= 7) {
                     this.treeifyBin(var5, var4);
                  }
               }

               ++this.modCount;
               ++this.size;
               this.afterNodeInsertion(true);
            }

            return var2;
         }
      }
   }

   public void forEach(BiConsumer var1) {
      if (var1 == null) {
         throw new NullPointerException();
      } else {
         if (this.size > 0) {
            HashMap.Node[] var2 = this.table;
            if (this.table != null) {
               int var3 = this.modCount;

               for(int var4 = 0; var4 < var2.length; ++var4) {
                  for(HashMap.Node var5 = var2[var4]; var5 != null; var5 = var5.next) {
                     var1.accept(var5.key, var5.value);
                  }
               }

               if (this.modCount != var3) {
                  throw new ConcurrentModificationException();
               }
            }
         }

      }
   }

   public void replaceAll(BiFunction var1) {
      if (var1 == null) {
         throw new NullPointerException();
      } else {
         if (this.size > 0) {
            HashMap.Node[] var2 = this.table;
            if (this.table != null) {
               int var3 = this.modCount;

               for(int var4 = 0; var4 < var2.length; ++var4) {
                  for(HashMap.Node var5 = var2[var4]; var5 != null; var5 = var5.next) {
                     var5.value = var1.apply(var5.key, var5.value);
                  }
               }

               if (this.modCount != var3) {
                  throw new ConcurrentModificationException();
               }
            }
         }

      }
   }

   public Object clone() {
      HashMap var1;
      try {
         var1 = (HashMap)super.clone();
      } catch (CloneNotSupportedException var3) {
         throw new InternalError(var3);
      }

      var1.reinitialize();
      var1.putMapEntries(this, false);
      return var1;
   }

   final float loadFactor() {
      return this.loadFactor;
   }

   final int capacity() {
      return this.table != null ? this.table.length : (this.threshold > 0 ? this.threshold : 16);
   }

   private void writeObject(ObjectOutputStream var1) throws IOException {
      int var2 = this.capacity();
      var1.defaultWriteObject();
      var1.writeInt(var2);
      var1.writeInt(this.size);
      this.internalWriteEntries(var1);
   }

   private void readObject(ObjectInputStream var1) throws IOException, ClassNotFoundException {
      var1.defaultReadObject();
      this.reinitialize();
      if (this.loadFactor > 0.0F && !Float.isNaN(this.loadFactor)) {
         var1.readInt();
         int var2 = var1.readInt();
         if (var2 < 0) {
            throw new InvalidObjectException("Illegal mappings count: " + var2);
         } else {
            if (var2 > 0) {
               float var3 = Math.min(Math.max(0.25F, this.loadFactor), 4.0F);
               float var4 = (float)var2 / var3 + 1.0F;
               int var5 = var4 < 16.0F ? 16 : (var4 >= 1.07374182E9F ? 1073741824 : tableSizeFor((int)var4));
               float var6 = (float)var5 * var3;
               this.threshold = var5 < 1073741824 && var6 < 1.07374182E9F ? (int)var6 : Integer.MAX_VALUE;
               SharedSecrets.getJavaOISAccess().checkArray(var1, Map.Entry[].class, var5);
               HashMap.Node[] var7 = (HashMap.Node[])(new HashMap.Node[var5]);
               this.table = var7;

               for(int var8 = 0; var8 < var2; ++var8) {
                  Object var9 = var1.readObject();
                  Object var10 = var1.readObject();
                  this.putVal(hash(var9), var9, var10, false, false);
               }
            }

         }
      } else {
         throw new InvalidObjectException("Illegal load factor: " + this.loadFactor);
      }
   }

   HashMap.Node newNode(int var1, Object var2, Object var3, HashMap.Node var4) {
      return new HashMap.Node(var1, var2, var3, var4);
   }

   HashMap.Node replacementNode(HashMap.Node var1, HashMap.Node var2) {
      return new HashMap.Node(var1.hash, var1.key, var1.value, var2);
   }

   HashMap.TreeNode newTreeNode(int var1, Object var2, Object var3, HashMap.Node var4) {
      return new HashMap.TreeNode(var1, var2, var3, var4);
   }

   HashMap.TreeNode replacementTreeNode(HashMap.Node var1, HashMap.Node var2) {
      return new HashMap.TreeNode(var1.hash, var1.key, var1.value, var2);
   }

   void reinitialize() {
      this.table = null;
      this.entrySet = null;
      this.keySet = null;
      this.values = null;
      this.modCount = 0;
      this.threshold = 0;
      this.size = 0;
   }

   void afterNodeAccess(HashMap.Node var1) {
   }

   void afterNodeInsertion(boolean var1) {
   }

   void afterNodeRemoval(HashMap.Node var1) {
   }

   void internalWriteEntries(ObjectOutputStream var1) throws IOException {
      if (this.size > 0) {
         HashMap.Node[] var2 = this.table;
         if (this.table != null) {
            for(int var3 = 0; var3 < var2.length; ++var3) {
               for(HashMap.Node var4 = var2[var3]; var4 != null; var4 = var4.next) {
                  var1.writeObject(var4.key);
                  var1.writeObject(var4.value);
               }
            }
         }
      }

   }

   static final class TreeNode extends LinkedHashMap.Entry {
      HashMap.TreeNode parent;
      HashMap.TreeNode left;
      HashMap.TreeNode right;
      HashMap.TreeNode prev;
      boolean red;

      TreeNode(int var1, Object var2, Object var3, HashMap.Node var4) {
         super(var1, var2, var3, var4);
      }

      final HashMap.TreeNode root() {
         HashMap.TreeNode var1 = this;

         while(true) {
            HashMap.TreeNode var2 = var1.parent;
            if (var1.parent == null) {
               return var1;
            }

            var1 = var2;
         }
      }

      static void moveRootToFront(HashMap.Node[] var0, HashMap.TreeNode var1) {
         int var2;
         if (var1 != null && var0 != null && (var2 = var0.length) > 0) {
            int var3 = var2 - 1 & var1.hash;
            HashMap.TreeNode var4 = (HashMap.TreeNode)var0[var3];
            if (var1 != var4) {
               var0[var3] = var1;
               HashMap.TreeNode var6 = var1.prev;
               HashMap.Node var5 = var1.next;
               if (var1.next != null) {
                  ((HashMap.TreeNode)var5).prev = var6;
               }

               if (var6 != null) {
                  var6.next = var5;
               }

               if (var4 != null) {
                  var4.prev = var1;
               }

               var1.next = var4;
               var1.prev = null;
            }

            assert checkInvariants(var1);
         }

      }

      final HashMap.TreeNode find(int var1, Object var2, Class var3) {
         HashMap.TreeNode var4 = this;

         do {
            HashMap.TreeNode var8 = var4.left;
            HashMap.TreeNode var9 = var4.right;
            int var5 = var4.hash;
            if (var4.hash > var1) {
               var4 = var8;
            } else if (var5 < var1) {
               var4 = var9;
            } else {
               Object var7 = var4.key;
               if (var4.key == var2 || var2 != null && var2.equals(var7)) {
                  return var4;
               }

               if (var8 == null) {
                  var4 = var9;
               } else if (var9 == null) {
                  var4 = var8;
               } else {
                  int var6;
                  if ((var3 != null || (var3 = HashMap.comparableClassFor(var2)) != null) && (var6 = HashMap.compareComparables(var3, var2, var7)) != 0) {
                     var4 = var6 < 0 ? var8 : var9;
                  } else {
                     HashMap.TreeNode var10;
                     if ((var10 = var9.find(var1, var2, var3)) != null) {
                        return var10;
                     }

                     var4 = var8;
                  }
               }
            }
         } while(var4 != null);

         return null;
      }

      final HashMap.TreeNode getTreeNode(int var1, Object var2) {
         return (this.parent != null ? this.root() : this).find(var1, var2, (Class)null);
      }

      static int tieBreakOrder(Object var0, Object var1) {
         int var2;
         if (var0 == null || var1 == null || (var2 = var0.getClass().getName().compareTo(var1.getClass().getName())) == 0) {
            var2 = System.identityHashCode(var0) <= System.identityHashCode(var1) ? -1 : 1;
         }

         return var2;
      }

      final void treeify(HashMap.Node[] var1) {
         HashMap.TreeNode var2 = null;

         HashMap.TreeNode var4;
         for(HashMap.TreeNode var3 = this; var3 != null; var3 = var4) {
            var4 = (HashMap.TreeNode)var3.next;
            var3.left = var3.right = null;
            if (var2 == null) {
               var3.parent = null;
               var3.red = false;
               var2 = var3;
            } else {
               Object var5 = var3.key;
               int var6 = var3.hash;
               Class var7 = null;
               HashMap.TreeNode var8 = var2;

               int var9;
               HashMap.TreeNode var12;
               do {
                  Object var11 = var8.key;
                  int var10 = var8.hash;
                  if (var8.hash > var6) {
                     var9 = -1;
                  } else if (var10 < var6) {
                     var9 = 1;
                  } else if (var7 == null && (var7 = HashMap.comparableClassFor(var5)) == null || (var9 = HashMap.compareComparables(var7, var5, var11)) == 0) {
                     var9 = tieBreakOrder(var5, var11);
                  }

                  var12 = var8;
               } while((var8 = var9 <= 0 ? var8.left : var8.right) != null);

               var3.parent = var12;
               if (var9 <= 0) {
                  var12.left = var3;
               } else {
                  var12.right = var3;
               }

               var2 = balanceInsertion(var2, var3);
            }
         }

         moveRootToFront(var1, var2);
      }

      final HashMap.Node untreeify(HashMap var1) {
         HashMap.Node var2 = null;
         HashMap.Node var3 = null;

         for(Object var4 = this; var4 != null; var4 = ((HashMap.Node)var4).next) {
            HashMap.Node var5 = var1.replacementNode((HashMap.Node)var4, (HashMap.Node)null);
            if (var3 == null) {
               var2 = var5;
            } else {
               var3.next = var5;
            }

            var3 = var5;
         }

         return var2;
      }

      final HashMap.TreeNode putTreeVal(HashMap var1, HashMap.Node[] var2, int var3, Object var4, Object var5) {
         Class var6 = null;
         boolean var7 = false;
         HashMap.TreeNode var8 = this.parent != null ? this.root() : this;
         HashMap.TreeNode var9 = var8;

         HashMap.TreeNode var13;
         while(true) {
            int var11 = var9.hash;
            int var10;
            if (var9.hash > var3) {
               var10 = -1;
            } else if (var11 < var3) {
               var10 = 1;
            } else {
               Object var12 = var9.key;
               if (var9.key == var4 || var4 != null && var4.equals(var12)) {
                  return var9;
               }

               if (var6 == null && (var6 = HashMap.comparableClassFor(var4)) == null || (var10 = HashMap.compareComparables(var6, var4, var12)) == 0) {
                  if (!var7) {
                     var7 = true;
                     HashMap.TreeNode var14 = var9.left;
                     if (var9.left != null && (var13 = var14.find(var3, var4, var6)) != null) {
                        break;
                     }

                     var14 = var9.right;
                     if (var9.right != null && (var13 = var14.find(var3, var4, var6)) != null) {
                        break;
                     }
                  }

                  var10 = tieBreakOrder(var4, var12);
               }
            }

            var13 = var9;
            if ((var9 = var10 <= 0 ? var9.left : var9.right) == null) {
               HashMap.Node var16 = var13.next;
               HashMap.TreeNode var15 = var1.newTreeNode(var3, var4, var5, var16);
               if (var10 <= 0) {
                  var13.left = var15;
               } else {
                  var13.right = var15;
               }

               var13.next = var15;
               var15.parent = var15.prev = var13;
               if (var16 != null) {
                  ((HashMap.TreeNode)var16).prev = var15;
               }

               moveRootToFront(var2, balanceInsertion(var8, var15));
               return null;
            }
         }

         return var13;
      }

      final void removeTreeNode(HashMap var1, HashMap.Node[] var2, boolean var3) {
         int var4;
         if (var2 != null && (var4 = var2.length) != 0) {
            int var5 = var4 - 1 & this.hash;
            HashMap.TreeNode var6 = (HashMap.TreeNode)var2[var5];
            HashMap.TreeNode var7 = var6;
            HashMap.TreeNode var9 = (HashMap.TreeNode)this.next;
            HashMap.TreeNode var10 = this.prev;
            if (var10 == null) {
               var6 = var9;
               var2[var5] = var9;
            } else {
               var10.next = var9;
            }

            if (var9 != null) {
               var9.prev = var10;
            }

            if (var6 != null) {
               if (var7.parent != null) {
                  var7 = var7.root();
               }

               if (var7 != null && var7.right != null) {
                  HashMap.TreeNode var8 = var7.left;
                  if (var7.left != null && var8.left != null) {
                     HashMap.TreeNode var12 = this.left;
                     HashMap.TreeNode var13 = this.right;
                     HashMap.TreeNode var14;
                     HashMap.TreeNode var15;
                     HashMap.TreeNode var16;
                     if (var12 != null && var13 != null) {
                        var15 = var13;

                        while(true) {
                           var16 = var15.left;
                           if (var15.left == null) {
                              boolean var17 = var15.red;
                              var15.red = this.red;
                              this.red = var17;
                              HashMap.TreeNode var18 = var15.right;
                              HashMap.TreeNode var19 = this.parent;
                              if (var15 == var13) {
                                 this.parent = var15;
                                 var15.right = this;
                              } else {
                                 HashMap.TreeNode var20 = var15.parent;
                                 if ((this.parent = var20) != null) {
                                    if (var15 == var20.left) {
                                       var20.left = this;
                                    } else {
                                       var20.right = this;
                                    }
                                 }

                                 if ((var15.right = var13) != null) {
                                    var13.parent = var15;
                                 }
                              }

                              this.left = null;
                              if ((this.right = var18) != null) {
                                 var18.parent = this;
                              }

                              if ((var15.left = var12) != null) {
                                 var12.parent = var15;
                              }

                              if ((var15.parent = var19) == null) {
                                 var7 = var15;
                              } else if (this == var19.left) {
                                 var19.left = var15;
                              } else {
                                 var19.right = var15;
                              }

                              if (var18 != null) {
                                 var14 = var18;
                              } else {
                                 var14 = this;
                              }
                              break;
                           }

                           var15 = var16;
                        }
                     } else if (var12 != null) {
                        var14 = var12;
                     } else if (var13 != null) {
                        var14 = var13;
                     } else {
                        var14 = this;
                     }

                     if (var14 != this) {
                        var15 = var14.parent = this.parent;
                        if (var15 == null) {
                           var7 = var14;
                        } else if (this == var15.left) {
                           var15.left = var14;
                        } else {
                           var15.right = var14;
                        }

                        this.left = this.right = this.parent = null;
                     }

                     var15 = this.red ? var7 : balanceDeletion(var7, var14);
                     if (var14 == this) {
                        var16 = this.parent;
                        this.parent = null;
                        if (var16 != null) {
                           if (this == var16.left) {
                              var16.left = null;
                           } else if (this == var16.right) {
                              var16.right = null;
                           }
                        }
                     }

                     if (var3) {
                        moveRootToFront(var2, var15);
                     }

                     return;
                  }
               }

               var2[var5] = var6.untreeify(var1);
            }
         }
      }

      final void split(HashMap var1, HashMap.Node[] var2, int var3, int var4) {
         HashMap.TreeNode var6 = null;
         HashMap.TreeNode var7 = null;
         HashMap.TreeNode var8 = null;
         HashMap.TreeNode var9 = null;
         int var10 = 0;
         int var11 = 0;

         HashMap.TreeNode var13;
         for(HashMap.TreeNode var12 = this; var12 != null; var12 = var13) {
            var13 = (HashMap.TreeNode)var12.next;
            var12.next = null;
            if ((var12.hash & var4) == 0) {
               if ((var12.prev = var7) == null) {
                  var6 = var12;
               } else {
                  var7.next = var12;
               }

               var7 = var12;
               ++var10;
            } else {
               if ((var12.prev = var9) == null) {
                  var8 = var12;
               } else {
                  var9.next = var12;
               }

               var9 = var12;
               ++var11;
            }
         }

         if (var6 != null) {
            if (var10 <= 6) {
               var2[var3] = var6.untreeify(var1);
            } else {
               var2[var3] = var6;
               if (var8 != null) {
                  var6.treeify(var2);
               }
            }
         }

         if (var8 != null) {
            if (var11 <= 6) {
               var2[var3 + var4] = var8.untreeify(var1);
            } else {
               var2[var3 + var4] = var8;
               if (var6 != null) {
                  var8.treeify(var2);
               }
            }
         }

      }

      static HashMap.TreeNode rotateLeft(HashMap.TreeNode var0, HashMap.TreeNode var1) {
         if (var1 != null) {
            HashMap.TreeNode var2 = var1.right;
            if (var1.right != null) {
               HashMap.TreeNode var4;
               if ((var4 = var1.right = var2.left) != null) {
                  var4.parent = var1;
               }

               HashMap.TreeNode var3;
               if ((var3 = var2.parent = var1.parent) == null) {
                  var0 = var2;
                  var2.red = false;
               } else if (var3.left == var1) {
                  var3.left = var2;
               } else {
                  var3.right = var2;
               }

               var2.left = var1;
               var1.parent = var2;
            }
         }

         return var0;
      }

      static HashMap.TreeNode rotateRight(HashMap.TreeNode var0, HashMap.TreeNode var1) {
         if (var1 != null) {
            HashMap.TreeNode var2 = var1.left;
            if (var1.left != null) {
               HashMap.TreeNode var4;
               if ((var4 = var1.left = var2.right) != null) {
                  var4.parent = var1;
               }

               HashMap.TreeNode var3;
               if ((var3 = var2.parent = var1.parent) == null) {
                  var0 = var2;
                  var2.red = false;
               } else if (var3.right == var1) {
                  var3.right = var2;
               } else {
                  var3.left = var2;
               }

               var2.right = var1;
               var1.parent = var2;
            }
         }

         return var0;
      }

      static HashMap.TreeNode balanceInsertion(HashMap.TreeNode var0, HashMap.TreeNode var1) {
         var1.red = true;

         while(true) {
            HashMap.TreeNode var2 = var1.parent;
            if (var1.parent == null) {
               var1.red = false;
               return var1;
            }

            if (!var2.red) {
               break;
            }

            HashMap.TreeNode var3 = var2.parent;
            if (var2.parent == null) {
               break;
            }

            HashMap.TreeNode var4 = var3.left;
            if (var2 == var3.left) {
               HashMap.TreeNode var5 = var3.right;
               if (var3.right != null && var5.red) {
                  var5.red = false;
                  var2.red = false;
                  var3.red = true;
                  var1 = var3;
               } else {
                  if (var1 == var2.right) {
                     var1 = var2;
                     var0 = rotateLeft(var0, var2);
                     var3 = (var2 = var2.parent) == null ? null : var2.parent;
                  }

                  if (var2 != null) {
                     var2.red = false;
                     if (var3 != null) {
                        var3.red = true;
                        var0 = rotateRight(var0, var3);
                     }
                  }
               }
            } else if (var4 != null && var4.red) {
               var4.red = false;
               var2.red = false;
               var3.red = true;
               var1 = var3;
            } else {
               if (var1 == var2.left) {
                  var1 = var2;
                  var0 = rotateRight(var0, var2);
                  var3 = (var2 = var2.parent) == null ? null : var2.parent;
               }

               if (var2 != null) {
                  var2.red = false;
                  if (var3 != null) {
                     var3.red = true;
                     var0 = rotateLeft(var0, var3);
                  }
               }
            }
         }

         return var0;
      }

      static HashMap.TreeNode balanceDeletion(HashMap.TreeNode var0, HashMap.TreeNode var1) {
         while(var1 != null && var1 != var0) {
            HashMap.TreeNode var2 = var1.parent;
            if (var1.parent == null) {
               var1.red = false;
               return var1;
            }

            if (var1.red) {
               var1.red = false;
               return var0;
            }

            HashMap.TreeNode var3 = var2.left;
            HashMap.TreeNode var5;
            HashMap.TreeNode var6;
            if (var2.left == var1) {
               HashMap.TreeNode var4 = var2.right;
               if (var2.right != null && var4.red) {
                  var4.red = false;
                  var2.red = true;
                  var0 = rotateLeft(var0, var2);
                  var2 = var1.parent;
                  var4 = var1.parent == null ? null : var2.right;
               }

               if (var4 == null) {
                  var1 = var2;
               } else {
                  var5 = var4.left;
                  var6 = var4.right;
                  if (var6 != null && var6.red || var5 != null && var5.red) {
                     if (var6 == null || !var6.red) {
                        if (var5 != null) {
                           var5.red = false;
                        }

                        var4.red = true;
                        var0 = rotateRight(var0, var4);
                        var2 = var1.parent;
                        var4 = var1.parent == null ? null : var2.right;
                     }

                     if (var4 != null) {
                        var4.red = var2 == null ? false : var2.red;
                        var6 = var4.right;
                        if (var4.right != null) {
                           var6.red = false;
                        }
                     }

                     if (var2 != null) {
                        var2.red = false;
                        var0 = rotateLeft(var0, var2);
                     }

                     var1 = var0;
                  } else {
                     var4.red = true;
                     var1 = var2;
                  }
               }
            } else {
               if (var3 != null && var3.red) {
                  var3.red = false;
                  var2.red = true;
                  var0 = rotateRight(var0, var2);
                  var2 = var1.parent;
                  var3 = var1.parent == null ? null : var2.left;
               }

               if (var3 == null) {
                  var1 = var2;
               } else {
                  var5 = var3.left;
                  var6 = var3.right;
                  if ((var5 == null || !var5.red) && (var6 == null || !var6.red)) {
                     var3.red = true;
                     var1 = var2;
                  } else {
                     if (var5 == null || !var5.red) {
                        if (var6 != null) {
                           var6.red = false;
                        }

                        var3.red = true;
                        var0 = rotateLeft(var0, var3);
                        var2 = var1.parent;
                        var3 = var1.parent == null ? null : var2.left;
                     }

                     if (var3 != null) {
                        var3.red = var2 == null ? false : var2.red;
                        var5 = var3.left;
                        if (var3.left != null) {
                           var5.red = false;
                        }
                     }

                     if (var2 != null) {
                        var2.red = false;
                        var0 = rotateRight(var0, var2);
                     }

                     var1 = var0;
                  }
               }
            }
         }

         return var0;
      }

      static boolean checkInvariants(HashMap.TreeNode var0) {
         HashMap.TreeNode var1 = var0.parent;
         HashMap.TreeNode var2 = var0.left;
         HashMap.TreeNode var3 = var0.right;
         HashMap.TreeNode var4 = var0.prev;
         HashMap.TreeNode var5 = (HashMap.TreeNode)var0.next;
         if (var4 != null && var4.next != var0) {
            return false;
         } else if (var5 != null && var5.prev != var0) {
            return false;
         } else if (var1 != null && var0 != var1.left && var0 != var1.right) {
            return false;
         } else if (var2 != null && (var2.parent != var0 || var2.hash > var0.hash)) {
            return false;
         } else if (var3 == null || var3.parent == var0 && var3.hash >= var0.hash) {
            if (var0.red && var2 != null && var2.red && var3 != null && var3.red) {
               return false;
            } else if (var2 != null && !checkInvariants(var2)) {
               return false;
            } else {
               return var3 == null || checkInvariants(var3);
            }
         } else {
            return false;
         }
      }
   }

   static final class EntrySpliterator extends HashMap.HashMapSpliterator implements Spliterator {
      EntrySpliterator(HashMap var1, int var2, int var3, int var4, int var5) {
         super(var1, var2, var3, var4, var5);
      }

      public HashMap.EntrySpliterator trySplit() {
         int var1 = this.getFence();
         int var2 = this.index;
         int var3 = var2 + var1 >>> 1;
         return var2 < var3 && this.current == null ? new HashMap.EntrySpliterator(this.map, var2, this.index = var3, this.est >>>= 1, this.expectedModCount) : null;
      }

      public void forEachRemaining(Consumer var1) {
         if (var1 == null) {
            throw new NullPointerException();
         } else {
            HashMap var5 = this.map;
            HashMap.Node[] var6 = var5.table;
            int var3 = this.fence;
            int var4;
            if (this.fence < 0) {
               var4 = this.expectedModCount = var5.modCount;
               var3 = this.fence = var6 == null ? 0 : var6.length;
            } else {
               var4 = this.expectedModCount;
            }

            if (var6 != null && var6.length >= var3) {
               int var2 = this.index;
               if (this.index >= 0 && (var2 < (this.index = var3) || this.current != null)) {
                  HashMap.Node var7 = this.current;
                  this.current = null;

                  do {
                     do {
                        if (var7 == null) {
                           var7 = var6[var2++];
                        } else {
                           var1.accept(var7);
                           var7 = var7.next;
                        }
                     } while(var7 != null);
                  } while(var2 < var3);

                  if (var5.modCount != var4) {
                     throw new ConcurrentModificationException();
                  }
               }
            }

         }
      }

      public boolean tryAdvance(Consumer var1) {
         if (var1 == null) {
            throw new NullPointerException();
         } else {
            HashMap.Node[] var3 = this.map.table;
            int var2;
            if (var3 != null && var3.length >= (var2 = this.getFence()) && this.index >= 0) {
               while(this.current != null || this.index < var2) {
                  if (this.current != null) {
                     HashMap.Node var4 = this.current;
                     this.current = this.current.next;
                     var1.accept(var4);
                     if (this.map.modCount != this.expectedModCount) {
                        throw new ConcurrentModificationException();
                     }

                     return true;
                  }

                  this.current = var3[this.index++];
               }
            }

            return false;
         }
      }

      public int characteristics() {
         return (this.fence >= 0 && this.est != this.map.size ? 0 : 64) | 1;
      }
   }

   static final class ValueSpliterator extends HashMap.HashMapSpliterator implements Spliterator {
      ValueSpliterator(HashMap var1, int var2, int var3, int var4, int var5) {
         super(var1, var2, var3, var4, var5);
      }

      public HashMap.ValueSpliterator trySplit() {
         int var1 = this.getFence();
         int var2 = this.index;
         int var3 = var2 + var1 >>> 1;
         return var2 < var3 && this.current == null ? new HashMap.ValueSpliterator(this.map, var2, this.index = var3, this.est >>>= 1, this.expectedModCount) : null;
      }

      public void forEachRemaining(Consumer var1) {
         if (var1 == null) {
            throw new NullPointerException();
         } else {
            HashMap var5 = this.map;
            HashMap.Node[] var6 = var5.table;
            int var3 = this.fence;
            int var4;
            if (this.fence < 0) {
               var4 = this.expectedModCount = var5.modCount;
               var3 = this.fence = var6 == null ? 0 : var6.length;
            } else {
               var4 = this.expectedModCount;
            }

            if (var6 != null && var6.length >= var3) {
               int var2 = this.index;
               if (this.index >= 0 && (var2 < (this.index = var3) || this.current != null)) {
                  HashMap.Node var7 = this.current;
                  this.current = null;

                  do {
                     do {
                        if (var7 == null) {
                           var7 = var6[var2++];
                        } else {
                           var1.accept(var7.value);
                           var7 = var7.next;
                        }
                     } while(var7 != null);
                  } while(var2 < var3);

                  if (var5.modCount != var4) {
                     throw new ConcurrentModificationException();
                  }
               }
            }

         }
      }

      public boolean tryAdvance(Consumer var1) {
         if (var1 == null) {
            throw new NullPointerException();
         } else {
            HashMap.Node[] var3 = this.map.table;
            int var2;
            if (var3 != null && var3.length >= (var2 = this.getFence()) && this.index >= 0) {
               while(this.current != null || this.index < var2) {
                  if (this.current != null) {
                     Object var4 = this.current.value;
                     this.current = this.current.next;
                     var1.accept(var4);
                     if (this.map.modCount != this.expectedModCount) {
                        throw new ConcurrentModificationException();
                     }

                     return true;
                  }

                  this.current = var3[this.index++];
               }
            }

            return false;
         }
      }

      public int characteristics() {
         return this.fence >= 0 && this.est != this.map.size ? 0 : 64;
      }
   }

   static final class KeySpliterator extends HashMap.HashMapSpliterator implements Spliterator {
      KeySpliterator(HashMap var1, int var2, int var3, int var4, int var5) {
         super(var1, var2, var3, var4, var5);
      }

      public HashMap.KeySpliterator trySplit() {
         int var1 = this.getFence();
         int var2 = this.index;
         int var3 = var2 + var1 >>> 1;
         return var2 < var3 && this.current == null ? new HashMap.KeySpliterator(this.map, var2, this.index = var3, this.est >>>= 1, this.expectedModCount) : null;
      }

      public void forEachRemaining(Consumer var1) {
         if (var1 == null) {
            throw new NullPointerException();
         } else {
            HashMap var5 = this.map;
            HashMap.Node[] var6 = var5.table;
            int var3 = this.fence;
            int var4;
            if (this.fence < 0) {
               var4 = this.expectedModCount = var5.modCount;
               var3 = this.fence = var6 == null ? 0 : var6.length;
            } else {
               var4 = this.expectedModCount;
            }

            if (var6 != null && var6.length >= var3) {
               int var2 = this.index;
               if (this.index >= 0 && (var2 < (this.index = var3) || this.current != null)) {
                  HashMap.Node var7 = this.current;
                  this.current = null;

                  do {
                     do {
                        if (var7 == null) {
                           var7 = var6[var2++];
                        } else {
                           var1.accept(var7.key);
                           var7 = var7.next;
                        }
                     } while(var7 != null);
                  } while(var2 < var3);

                  if (var5.modCount != var4) {
                     throw new ConcurrentModificationException();
                  }
               }
            }

         }
      }

      public boolean tryAdvance(Consumer var1) {
         if (var1 == null) {
            throw new NullPointerException();
         } else {
            HashMap.Node[] var3 = this.map.table;
            int var2;
            if (var3 != null && var3.length >= (var2 = this.getFence()) && this.index >= 0) {
               while(this.current != null || this.index < var2) {
                  if (this.current != null) {
                     Object var4 = this.current.key;
                     this.current = this.current.next;
                     var1.accept(var4);
                     if (this.map.modCount != this.expectedModCount) {
                        throw new ConcurrentModificationException();
                     }

                     return true;
                  }

                  this.current = var3[this.index++];
               }
            }

            return false;
         }
      }

      public int characteristics() {
         return (this.fence >= 0 && this.est != this.map.size ? 0 : 64) | 1;
      }
   }

   static class HashMapSpliterator {
      final HashMap map;
      HashMap.Node current;
      int index;
      int fence;
      int est;
      int expectedModCount;

      HashMapSpliterator(HashMap var1, int var2, int var3, int var4, int var5) {
         super();
         this.map = var1;
         this.index = var2;
         this.fence = var3;
         this.est = var4;
         this.expectedModCount = var5;
      }

      final int getFence() {
         int var1 = this.fence;
         if (this.fence < 0) {
            HashMap var2 = this.map;
            this.est = var2.size;
            this.expectedModCount = var2.modCount;
            HashMap.Node[] var3 = var2.table;
            var1 = this.fence = var3 == null ? 0 : var3.length;
         }

         return var1;
      }

      public final long estimateSize() {
         this.getFence();
         return (long)this.est;
      }
   }

   final class EntryIterator extends HashMap.HashIterator implements Iterator {
      EntryIterator() {
         super();
      }

      public final Map.Entry next() {
         return this.nextNode();
      }
   }

   final class ValueIterator extends HashMap.HashIterator implements Iterator {
      ValueIterator() {
         super();
      }

      public final Object next() {
         return this.nextNode().value;
      }
   }

   final class KeyIterator extends HashMap.HashIterator implements Iterator {
      KeyIterator() {
         super();
      }

      public final Object next() {
         return this.nextNode().key;
      }
   }

   abstract class HashIterator {
      HashMap.Node next;
      HashMap.Node current;
      int expectedModCount;
      int index;

      HashIterator() {
         super();
         this.expectedModCount = HashMap.this.modCount;
         HashMap.Node[] var2 = HashMap.this.table;
         this.current = this.next = null;
         this.index = 0;
         if (var2 != null && HashMap.this.size > 0) {
            while(this.index < var2.length && (this.next = var2[this.index++]) == null) {
               ;
            }
         }

      }

      public final boolean hasNext() {
         return this.next != null;
      }

      final HashMap.Node nextNode() {
         HashMap.Node var2 = this.next;
         if (HashMap.this.modCount != this.expectedModCount) {
            throw new ConcurrentModificationException();
         } else if (var2 == null) {
            throw new NoSuchElementException();
         } else {
            if ((this.next = (this.current = var2).next) == null) {
               HashMap.Node[] var1 = HashMap.this.table;
               if (HashMap.this.table != null) {
                  while(this.index < var1.length && (this.next = var1[this.index++]) == null) {
                     ;
                  }
               }
            }

            return var2;
         }
      }

      public final void remove() {
         HashMap.Node var1 = this.current;
         if (var1 == null) {
            throw new IllegalStateException();
         } else if (HashMap.this.modCount != this.expectedModCount) {
            throw new ConcurrentModificationException();
         } else {
            this.current = null;
            Object var2 = var1.key;
            HashMap.this.removeNode(HashMap.hash(var2), var2, (Object)null, false, false);
            this.expectedModCount = HashMap.this.modCount;
         }
      }
   }

   final class EntrySet extends AbstractSet {
      EntrySet() {
         super();
      }

      public final int size() {
         return HashMap.this.size;
      }

      public final void clear() {
         HashMap.this.clear();
      }

      public final Iterator iterator() {
         return HashMap.this.new EntryIterator();
      }

      public final boolean contains(Object var1) {
         if (!(var1 instanceof Map.Entry)) {
            return false;
         } else {
            Map.Entry var2 = (Map.Entry)var1;
            Object var3 = var2.getKey();
            HashMap.Node var4 = HashMap.this.getNode(HashMap.hash(var3), var3);
            return var4 != null && var4.equals(var2);
         }
      }

      public final boolean remove(Object var1) {
         if (var1 instanceof Map.Entry) {
            Map.Entry var2 = (Map.Entry)var1;
            Object var3 = var2.getKey();
            Object var4 = var2.getValue();
            return HashMap.this.removeNode(HashMap.hash(var3), var3, var4, true, true) != null;
         } else {
            return false;
         }
      }

      public final Spliterator spliterator() {
         return new HashMap.EntrySpliterator(HashMap.this, 0, -1, 0, 0);
      }

      public final void forEach(Consumer var1) {
         if (var1 == null) {
            throw new NullPointerException();
         } else {
            if (HashMap.this.size > 0) {
               HashMap.Node[] var2 = HashMap.this.table;
               if (HashMap.this.table != null) {
                  int var3 = HashMap.this.modCount;

                  for(int var4 = 0; var4 < var2.length; ++var4) {
                     for(HashMap.Node var5 = var2[var4]; var5 != null; var5 = var5.next) {
                        var1.accept(var5);
                     }
                  }

                  if (HashMap.this.modCount != var3) {
                     throw new ConcurrentModificationException();
                  }
               }
            }

         }
      }
   }

   final class Values extends AbstractCollection {
      Values() {
         super();
      }

      public final int size() {
         return HashMap.this.size;
      }

      public final void clear() {
         HashMap.this.clear();
      }

      public final Iterator iterator() {
         return HashMap.this.new ValueIterator();
      }

      public final boolean contains(Object var1) {
         return HashMap.this.containsValue(var1);
      }

      public final Spliterator spliterator() {
         return new HashMap.ValueSpliterator(HashMap.this, 0, -1, 0, 0);
      }

      public final void forEach(Consumer var1) {
         if (var1 == null) {
            throw new NullPointerException();
         } else {
            if (HashMap.this.size > 0) {
               HashMap.Node[] var2 = HashMap.this.table;
               if (HashMap.this.table != null) {
                  int var3 = HashMap.this.modCount;

                  for(int var4 = 0; var4 < var2.length; ++var4) {
                     for(HashMap.Node var5 = var2[var4]; var5 != null; var5 = var5.next) {
                        var1.accept(var5.value);
                     }
                  }

                  if (HashMap.this.modCount != var3) {
                     throw new ConcurrentModificationException();
                  }
               }
            }

         }
      }
   }

   final class KeySet extends AbstractSet {
      KeySet() {
         super();
      }

      public final int size() {
         return HashMap.this.size;
      }

      public final void clear() {
         HashMap.this.clear();
      }

      public final Iterator iterator() {
         return HashMap.this.new KeyIterator();
      }

      public final boolean contains(Object var1) {
         return HashMap.this.containsKey(var1);
      }

      public final boolean remove(Object var1) {
         return HashMap.this.removeNode(HashMap.hash(var1), var1, (Object)null, false, true) != null;
      }

      public final Spliterator spliterator() {
         return new HashMap.KeySpliterator(HashMap.this, 0, -1, 0, 0);
      }

      public final void forEach(Consumer var1) {
         if (var1 == null) {
            throw new NullPointerException();
         } else {
            if (HashMap.this.size > 0) {
               HashMap.Node[] var2 = HashMap.this.table;
               if (HashMap.this.table != null) {
                  int var3 = HashMap.this.modCount;

                  for(int var4 = 0; var4 < var2.length; ++var4) {
                     for(HashMap.Node var5 = var2[var4]; var5 != null; var5 = var5.next) {
                        var1.accept(var5.key);
                     }
                  }

                  if (HashMap.this.modCount != var3) {
                     throw new ConcurrentModificationException();
                  }
               }
            }

         }
      }
   }

   static class Node implements Map.Entry {
      final int hash;
      final Object key;
      Object value;
      HashMap.Node next;

      Node(int var1, Object var2, Object var3, HashMap.Node var4) {
         super();
         this.hash = var1;
         this.key = var2;
         this.value = var3;
         this.next = var4;
      }

      public final Object getKey() {
         return this.key;
      }

      public final Object getValue() {
         return this.value;
      }

      public final String toString() {
         return this.key + "=" + this.value;
      }

      public final int hashCode() {
         return Objects.hashCode(this.key) ^ Objects.hashCode(this.value);
      }

      public final Object setValue(Object var1) {
         Object var2 = this.value;
         this.value = var1;
         return var2;
      }

      public final boolean equals(Object var1) {
         if (var1 == this) {
            return true;
         } else {
            if (var1 instanceof Map.Entry) {
               Map.Entry var2 = (Map.Entry)var1;
               if (Objects.equals(this.key, var2.getKey()) && Objects.equals(this.value, var2.getValue())) {
                  return true;
               }
            }

            return false;
         }
      }
   }
}

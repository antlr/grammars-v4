public class RecordsTesting {
    public record BindingsSnapshot(int a) { }

    private record OtherRecord(int b) {
        protected OtherRecord {
            ;
        }
    }

    record NextRecord(int c) {
        public NextRecord(int c) throws java.lang.RuntimeException {
            this.c = c;
        }
    }
    interface InnerInterface{
        record PoolKey(Name name, BsmKey bsmKey, Object dynamicType) {}
    }
}


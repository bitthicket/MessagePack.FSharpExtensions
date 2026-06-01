using MessagePack.Formatters;
using Microsoft.FSharp.Core;

namespace MessagePack.FSharp.Formatters
{
    public sealed class UnitFormatter : IMessagePackFormatter<Unit?>
    {

        public UnitFormatter() { }

        public void Serialize(ref MessagePackWriter writer, Unit? value, MessagePackSerializerOptions options)
        {
            writer.WriteNil();
            return;
        }

        public Unit? Deserialize(ref MessagePackReader reader, MessagePackSerializerOptions options)
        {
            reader.ReadNil();
            // null is the correct CLR representation of F# unit in generic type contexts.
            return null;
        }
    }
}

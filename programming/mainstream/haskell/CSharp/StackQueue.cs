using System.Collections;
using System.Collections.Generic;

public class StackQueue<T> : IEnumerable<T>
{
    private readonly List<T> _items;

    public StackQueue()
    {
        _items = new List<T>();
    }

    public StackQueue(T value)
    {
        _items = new List<T>();
        _items.Add(value);
    }

    public StackQueue(StackQueue<T> other)
    {
        _items = new List<T>();
        _items.AddRange(other._items);
    }

    public virtual int Size()
    {
        return _items.Count;
    }

    public virtual int Count => _items.Count;

    public virtual T Pop()
    {
        if (_items.Count > 0)
        {
            var result = _items[_items.Count - 1];
            _items.RemoveAt(_items.Count - 1);
            return result;
        }
        else return default;
    }

    public virtual T this[int n]
    {
        get => PeekBottom(n);
        set => _items[n] = value;
    }

    public virtual T Peek()
    {
        return PeekTop(0);
    }

    public bool Any()
    {
        return _items.Count > 0;
    }

    public virtual T PeekTop(int n = 0)
    {
        if (_items.Count - n > 0)
        {
            int index = _items.Count - n - 1;
            var result = _items[index];
            return result;
        }
        else return default;
    }

    public virtual T PeekBottom(int n)
    {
        if (n >= 0 && n < _items.Count - 1)
        {
            var result = _items[n];
            return result;
        }
        else return default;
    }

    public virtual void Push(T value)
    {
        _items.Add(value);
    }

    public virtual void Push(IEnumerable<T> collection)
    {
        foreach (T t in collection)
        {
            _items.Add(t);
        }
    }

    public virtual void PushMultiple(params T[] values)
    {
        int count = values.Length;
        for (int i = 0; i < count; i++) _items.Add(values[i]);
    }

    public virtual void EnqueueTop(T value)
    {
        Push(value);
    }

    public virtual void EnqueueBottom(T value)
    {
        _items.Insert(0, value);
    }

    public virtual T DequeueTop()
    {
        return Pop();
    }

    public virtual T DequeueBottom()
    {
        if (_items.Count > 0)
        {
            var result = _items[0];
            _items.RemoveAt(0);
            return result;
        }
        else return default;
    }

    public virtual bool Contains(T item)
    {
        return _items.Contains(item);
    }

    public virtual System.Collections.Generic.IEnumerator<T> GetEnumerator()
    {
        for (int i = _items.Count - 1; i >= 0; i--)
            yield return _items[i];
    }

    IEnumerator IEnumerable.GetEnumerator()
    {
        for (int i = _items.Count - 1; i >= 0; i--)
            yield return _items[i];
    }
}

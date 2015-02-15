// Example class

// (setq srefactor-use-srecode-p nil)
// (setq srefactor-use-srecode-p t)

class ColoringSolver
{ // -!-
private:
    std::set<std::set<std::map<int,int>>> *color_repo;
    std::map<int, int> color_statistics;

    class test {
        int test3(int a);
        template <typename T4, typename T5>
        class test2 {
            T5 test5(T4 t);
            int test4(int b);
        };
    };

    void find_used_colors(Vertex& v, std::set<int>& used_colors);
    void find_viable_colors(Vertex& v, const std::set<int>& used_colors, std::set<int>& viable_colors);
    int get_highest_color(const std::set<int>& viable_colors);
    int find_redundant_color(const Vertex& v, const std::set<int>& viable_color);
    void repaint_neighbors(const Vertex& v);
    void paint_indirect_nodes(const Vertex& v);
    void add_new_color(Vertex& v);
public:
    std::vector<Vertex> vertices;

    Answer solve();

    ColoringSolver(){}
    virtual ~ColoringSolver(){}
};

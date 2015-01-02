#include <cstdio>
#include <vector>

using std::vector;

class IntMatrix {
public:
  IntMatrix(int height, int width) :
    height_(height), width_(width) {
    matrix_.resize(height * width);
    std::fill(matrix_.begin(), matrix_.end(), 0);
  }
  
  const int& operator()(int y, int x) const {
    return matrix_[y * width_ + x];
  }

  void Set(int y, int x, int value) {
    matrix_[y * width_ + x] = value;
  }

  int Height() const {
    return height_;
  }

  int Width() const {
    return width_;
  }

  void DebugPrint() const {
    int k = 0;
    for (int i = 0; i < height_; ++i) {
      for (int j = 0; j < width_; ++j) {
        printf("%5d", matrix_[k++]);
      }
      printf("\n");
    }
  }
  
private:
  int height_;
  int width_;
  vector<int> matrix_;
};


int MaximizeMahattan(const IntMatrix &down, const IntMatrix &right) {
  int height = right.Height();
  int width = down.Width();
  IntMatrix result(height, width);
  for (int j = 1; j < width; ++j) {
    result.Set(0, j, result(0, j - 1) + right(0, j - 1));
  }
  
  for (int i = 1; i < height; ++ i) {
    result.Set(i, 0, result(i - 1, 0) + down(i - 1, 0));
  }
  
  for (int i = 1; i < height; ++i) {
    for (int j = 1; j < width; ++j) { 
      int left = result(i, j - 1) + right(i, j - 1);
      int up = result(i - 1, j) + down(i - 1, j);
      result.Set(i, j, left > up ? left : up);
    }
  }
  return result(height - 1, width - 1);
}

int main(int argc, char **argv) {
  int height = 0;
  int width = 0;
  scanf("%d", &height);
  scanf("%d", &width);
  ++height;
  ++width;

  int tmp = 0;
  IntMatrix down(height - 1, width);
  for (int i = 0; i < height - 1; ++i) {
    for (int j = 0; j < width; ++j) {
      scanf("%d", &tmp);
      down.Set(i, j, tmp);
    }
  }
  
  // Skip '-'
  char ch;
  do {
    scanf("%c", &ch); 
  } while ('-' != ch);


  IntMatrix right(height, width - 1);
  for (int i = 0; i < height; ++i) {
    for (int j = 0; j < width - 1; ++j) {
      scanf("%d", &tmp);
      right.Set(i, j, tmp);
    }
  }

  printf("%d\n", MaximizeMahattan(down, right));
  
  return 0;
}
